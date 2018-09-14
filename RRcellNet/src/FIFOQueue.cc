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

#include "FIFOQueue.h"

Define_Module(FIFOQueue);

void FIFOQueue::initialize()
{
   inData_p = gate("inData_p");
   outData_p = gate("outData_p");

   // self message is used for packetCount signaling
   SELF_MESSAGE_DELAY = par("packetCountBucket");
   self_message = new cMessage("self_message_FIFO");
   packetCount_s = registerSignal("packetCount");

   EV << getFullPath() << endl;
   EV << "delay at " << SELF_MESSAGE_DELAY << endl;
   scheduleAt(simTime() + SELF_MESSAGE_DELAY, self_message);
}

void FIFOQueue::handleMessage(cMessage *msg)
{
    if( msg->getArrivalGate() == inData_p ) // il webServer ha mandato il messaggio
    {
      //  EV << "packet queued" << endl;
        queue.insert((cPacket*)msg);
    } else if( msg->isSelfMessage()) {
        //signal for E[N]
        EV << "emitting packetCount" << endl;
        if(simTime() > getSimulation()->getWarmupPeriod())
            emit(packetCount_s, queue.getLength());
        scheduleAt(simTime() + SELF_MESSAGE_DELAY, self_message);
    }
}

cPacket* FIFOQueue::getPacket()
{
    cPacket *pkt = queue.front();
    return pkt;
}

cPacket* FIFOQueue::popFront(){
    if(queue.isEmpty())
        return nullptr;
    return queue.pop();
}
