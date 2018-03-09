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
    nSlotsFrame = par("nSlotsFrame");
   // timeSlotPeriod = par("timeSlotPeriod");
    timeFramePeriod = par("timeFramePeriod");
    inData_p = gate("inData_p");
    outCQI_p = gate("outCQI_p");
    idUser = idUser_counter++;
  //  EV << "MS time:" << simTime()+timeSlotPeriod*(nSlotsFrame+1) << endl;
    EV << "MS time:" << simTime()+timeFramePeriod << endl;
  //  scheduleAt(simTime()+(timeSlotPeriod/1000)*(nSlotsFrame+1),beepMS);
    scheduleAt(simTime()+timeFramePeriod/1000, beepMS);
}

void MobileStation::handleMessage(cMessage *msg)
{
    if( msg->isSelfMessage() ){
        cMessage *cqiMSG = new cMessage("cqiMSG");

        cMsgPar *idUserPar = new cMsgPar("idUser");
        idUserPar->setLongValue(idUser);
        cMsgPar *cqiPar = new cMsgPar("CQI");
        cqiPar->setLongValue(intuniform(1,15)); // per ora solo distribuz uniforme

        cqiMSG->addPar(idUserPar);
        cqiMSG->addPar(cqiPar);
        send(cqiMSG,outCQI_p);
       // scheduleAt(simTime()+(timeSlotPeriod/1000)*(nSlotsFrame+1),beepMS);
        scheduleAt(simTime()+timeFramePeriod/1000, beepMS);
    } else {
        EV << "pkt received " << msg->getName() << endl;
    }
}
