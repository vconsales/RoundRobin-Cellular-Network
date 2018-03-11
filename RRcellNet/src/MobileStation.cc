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

#include "MobileStation.h"

Define_Module(MobileStation);

unsigned int MobileStation::idUser_counter = 0;

void MobileStation::initialize()
{
  //  idUser = par("idUser");
    beepMS = new cMessage("beepMS");
    nFrameSlots = par("nFrameSlots");
    timeFramePeriod = par("timeFramePeriod");
    inData_p = gate("inData_p");
    outCQI_p = gate("outCQI_p");
    idUser = idUser_counter++;

    receivedPacket = receivedBytes = 0;
    EV << "MS time:" << simTime()+timeFramePeriod << endl;

    receivedBytes_s = registerSignal("receivedBytes");

  //  scheduleAt(simTime()+(timeSlotPeriod/1000)*(nFrameSlots+1),beepMS);
    scheduleAt(simTime(), beepMS);
}

void MobileStation::handleMessage(cMessage *msg)
{
    int lastId = -1;
    if( msg->isSelfMessage() ){
        EV << "idUser:" << idUser << " ReceivedBytes: " << receivedBytes << " ReceivedPacket:" << receivedPacket << endl;

        cMessage *cqiMSG = new cMessage("cqiMSG");

        cMsgPar *idUserPar = new cMsgPar("idUser");
        idUserPar->setLongValue(idUser);
        cMsgPar *cqiPar = new cMsgPar("CQI");
        cqiPar->setLongValue(intuniform(1,15)); // per ora solo distribuz uniforme

        cqiMSG->addPar(idUserPar);
        cqiMSG->addPar(cqiPar);
        send(cqiMSG,outCQI_p);

        scheduleAt(simTime()+timeFramePeriod/1000, beepMS);
    } else {
      //  EV << "pkt received " << msg->getName() << endl;
        FrameChunk *fchunk = check_and_cast<FrameChunk *>(msg);
        /*
        if( rb->getPacketId() != lastId )
            ++receivedPacket;
        receivedBytes += rb->getSizeByte();
        emit(receivedBytes_s,rb->getSizeByte());
        lastId = rb->getPacketId();*/
        delete fchunk;
    }
}
