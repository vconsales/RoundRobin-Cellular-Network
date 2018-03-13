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
#include <assert.h>
#include "FrameChunk.h"

Define_Module(Scheduler);

void Scheduler::initialize()
{
    nUsers = par("nUsers");
    nFrameSlots = par("nFrameSlots");
    timeFramePeriod = par("timeFramePeriod");
    CQI_users = new int[nUsers];
    memset(CQI_users,0,sizeof(int)*nUsers);

    currentUser = 0;
    beepSched = new cMessage("beepScheduler");

    char buf[100];
    for(int i=0; i<nUsers; i++){
        vec_outData.push_back(gate("outDataSched_p",i));
        EV << "vec_outData[i]: "<< vec_outData[i] << endl;

        sprintf(buf,"CellularNetwork.antenna.queue[%d]",i);
        cModule *m = getModuleByPath(buf);
        FIFOQueue *f = check_and_cast<FIFOQueue *>(m);
        vec_q.push_back(f);
    }

    //TODO: RISOLVERE PROBLEMA DELLA RICEZIONE DI TUTTI I CQI PRIMA DI COMPORRE IL FRAME
    scheduleAt(simTime()+0.0001f, beepSched);
}

int integerRoundDivision(const int n, const int d)
{
    return ((n < 0) ^ (d < 0)) ? ((n - d/2)/d) : ((n + d/2)/d);
}

void Scheduler::handleMessage(cMessage *msg)
{
    if( msg->isSelfMessage() ){
        sendRBs();

        // see you at the next timeslot...
        scheduleAt(simTime()+(timeFramePeriod/1000), beepSched);

    } else if( strcmp(msg->getName(),"cqiMSG") == 0 ){
        updateCQIs(msg);
    }
}

Scheduler::~Scheduler(){
  delete[] CQI_users;
  this->cancelAndDelete(beepSched);
}

int Scheduler::nextUser(){
    return currentUser=(currentUser+1)%nUsers;
}

void Scheduler::updateCQIs(cMessage *msg)
{
    cArray parList = msg->getParList();
   // EV <<"parList size:"<< parList.size() << endl;
    int idUser = ((cMsgPar*)parList[0])->longValue();
    int CQI = ((cMsgPar*)parList[1])->longValue();
    CQI_users[idUser] = CQI;
    assert(CQI != 0);
    EV << "updateCQIs: idUser=" << idUser << " CQI=" << CQI << endl;
    delete msg;
}

void Scheduler::sendRBs()
{
    EV << "scheduler self2" << endl;

    // the frame is composed cycling all the users until it is filled.
    // However if the packets of all users are not enough to fill the frame
    // we will start an infinite cycle: this variable is used to cycle the user
    // just one time
    int remainingUserCycles = nUsers;

    // the user we are working on is currentUser, but we need to cycle the
    // other users to eventually fill the remaining frame space, without
    // touching currentUser member
    int nowServingUser = currentUser;

    // we need to fill all the RBs
    int freeRBs = nFrameSlots;
    while(freeRBs)
    {
        // depending on the (user related) CQI and the RB count
        // we can compute the total available space in frame
        assert(nowServingUser >= 0 && nowServingUser < nUsers);
        int curCQI = CQI_users[nowServingUser];

        // ## !! this is an assertion to check that CQI is sent by all the users
        assert(curCQI!=0);
        CQI_users[nowServingUser] = 0; // current CQI is already in curCQI value
        // ##

        int RBbytes = CQI_B[curCQI];
        int freeFrameBytes = RBbytes*freeRBs;

        // we will send a FrameChunk to the user
        FrameChunk *fchunk = new FrameChunk(nUsers - remainingUserCycles, RBbytes);

        // fetch packet by packet from currentUser queue
        for(cPacket *pkt = vec_q[nowServingUser]->getPacket();
                pkt != nullptr; pkt = vec_q[nowServingUser]->getPacket())
        {
            int pktSize = pkt->getByteLength();
            EV << "Scheduler: pkt size=" << pktSize << " freeFrameBytes=" << freeFrameBytes
                    << " RBbytes=" << RBbytes <<endl;
            if(pktSize <= freeFrameBytes)
            {
                freeFrameBytes -= pktSize;
                // remove the packet from the queue and push it into the FrameChunk
                fchunk->insertPacket(vec_q[nowServingUser]->popFront());
            }
            else // not schedulable
                break;  // we must stop the schedulation because of the FIFO rule
        }

        // if we are here the current user queue is empty.
        // at this point we have just computed the frame allocation in terms of bytes,
        // we must convert it in terms of allocated RBs
        int allocatedFrameSpace = RBbytes*freeRBs - freeFrameBytes;
        assert(allocatedFrameSpace >= 0);

        // we use the round() function because a partially allocated RB must be considered
        // as allocated and must not be used by the next user
        assert(RBbytes!=0);
        int allocatedRbs = integerRoundDivision(allocatedFrameSpace, RBbytes);
        EV << "Scheduler: allocatedRB = " << allocatedRbs << endl;

        freeRBs -= allocatedRbs;

        // now we can send the FrameChunk to the current user if it contains at least
        // one packet (the condition is just for a visual debugging purpose)
        if(fchunk->packetCount() != 0)
            send(fchunk, vec_outData[nowServingUser]);
        else
            delete fchunk;

        assert(freeRBs >= 0);

        // we must cycle every user just one time
        if(--remainingUserCycles == 0)
            break;


        EV << "Scheduler: moving to next user" << endl;

        // we need to fill the frame, so we can fetch the packets from the next user
        // TODO: THIS IS A RR POLICY. HERE WE CAN CHANGE HOW THE USERS
        //       ARE CHOOSEN FOR THE REMAINING FRAME SPACE FILLING.
        nowServingUser = (nowServingUser+1)%nUsers;

    }

    // the next frame composing will work on the next user, following the Round Robin policy.
    nextUser();
}
