//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU Lesser General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
// 
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser General Public License for more details.
// 
// You should have received a copy of the GNU Lesser General Public License
// along with this program.  If not, see http://www.gnu.org/licenses/.
// 

#include "Scheduler.h"

Define_Module(Scheduler);

void Scheduler::initialize()
{
    nUsers = par("nUsers");
    nSlotsFrame = par("nSlotsFrame");
   // timeSlotPeriod = par("timeSlotPeriod");
    timeFramePeriod = par("timeFramePeriod");
    CQI_users = new int[nUsers];
    memset(CQI_users,0,sizeof(int)*nUsers);

    currentUser = 0;
    freeSlots = nSlotsFrame;
    beepSched = new cMessage("beepScheduler");

    char buf[100];
    for(int i=0; i<nUsers; i++){
        vec_outData.push_back(gate("outDataSched_p",i));
        EV << "vec_outData[i]: "<< vec_outData[i] << endl;

        sprintf(buf,"CellularNetwork.antenna.queue[%d]",i);
        cModule *m = getModuleByPath(buf);
        FIFOQueue *f = check_and_cast<FIFOQueue *>(m);
        vec_q.push_back(f);

        sendIdUser(i);
    }
   // EV << "scheduler t_beep:" << simTime()+timeSlotPeriod*(nSlotsFrame+1) << endl;
    // 1 slot è fittizio e serve per ricevere i CQI
  //  scheduleAt(simTime()+(timeSlotPeriod/1000)*(nSlotsFrame+1), beepSched);
    scheduleAt(simTime()+(timeFramePeriod/1000), beepSched);
}

void Scheduler::handleMessage(cMessage *msg)
{
    if( msg->isSelfMessage() ){
        EV << "scheduler self" << endl;
        while( freeSlots>0 ){
            int curCQI = CQI_users[currentUser];
            EV << "currentUser" << currentUser << "curCQI" << curCQI << endl;
            cPacket *pkt = vec_q[currentUser]->getPacket();
            EV << "pkt: " << pkt << endl;
            if( pkt != nullptr ){
                int pktSize = pkt->getByteLength();
                int bytesRB = CQI_B[curCQI];
                int nRBs = pktSize/bytesRB + pktSize%bytesRB;
                EV <<"pktSize:" << pktSize << "bytesRB" << bytesRB << "nRBs" << nRBs << endl;
                // da aggiustare. un pacchetto che non può essere trasmesso intero non va schedulato
                for(int i=0; i<nRBs && freeSlots>0; i++, freeSlots--)
                {
                    ResourceBlock *rb = new ResourceBlock(pkt->getId(),curCQI);
                  //  EV << "send resource block" <<endl;
                    send(rb,vec_outData[currentUser]);
                }

                vec_q[currentUser]->popFront();
            } else { // la coda è vuota
               // currentUser = nextUser();
                EV << "pkt=nullptr" << endl;
                break;
            }
        }
        freeSlots = nSlotsFrame;
        currentUser = nextUser();
        //scheduleAt(simTime()+(timeSlotPeriod/1000)*(nSlotsFrame+1), beepSched);
        scheduleAt(simTime()+(timeFramePeriod/1000), beepSched);
    } else if( strcmp(msg->getName(),"cqiMSG") == 0 ){
        cArray parList = msg->getParList();
        EV <<"parList size:"<< parList.size() << endl;
        int idUser = ((cMsgPar*)parList[0])->longValue();
        int CQI = ((cMsgPar*)parList[1])->longValue();
        CQI_users[idUser] = CQI;
        EV << "idUser: " << idUser << " CQI:" << CQI << endl;
    }
}

Scheduler::~Scheduler(){
  delete[] CQI_users;
}

int Scheduler::nextUser(){
    return (currentUser+1)%nUsers;
}

void Scheduler::sendIdUser(int id)
{
    cMessage *idMSG = new cMessage("idUserMSG");
    cMsgPar *idUserPar = new cMsgPar("idUser");
    idUserPar->setLongValue(id);
    idMSG->addPar(idUserPar);
    send(idMSG,vec_outData[id]);
}
