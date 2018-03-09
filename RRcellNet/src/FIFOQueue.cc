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

   EV << getFullPath() << endl;
}

void FIFOQueue::handleMessage(cMessage *msg)
{
    if( msg->getArrivalGate() == inData_p ) // il webServer ha mandato il messaggio
    {
      //  EV << "packet queued" << endl;
        queue.insert((cPacket*)msg);
    } /*else if ( msg->getArrivalGate() == reqData_p ){
        if( !queue.isEmpty() ){
            cPacket *pkt = queue.front();
            send(pkt,outData_p);
        } else {
            cMessage *empty = new cMessage("empty");
            send(empty,outData_p);
        }
    } else if( msg->getArrivalGate() == ackSched_p) {
        if( !queue.isEmpty() )
            queue.pop();
    }*/
}

cPacket* FIFOQueue::getPacket()
{
    cPacket *pkt = queue.front();
    return pkt;
}

bool FIFOQueue::popFront(){
    if( !queue.isEmpty() ){
        queue.pop();
        return true;
    } else
        return false;
}
