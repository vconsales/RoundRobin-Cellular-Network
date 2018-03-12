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

#include "WebServer.h"
#include "UserPacket_m.h"

Define_Module(WebServer);

void WebServer::initialize()
{
    lambda = par("lambda");
    beep = new cMessage("beep");
    simtime_t t0 = exponential((1/lambda),0)/1000;//da sistemare il RNG
    scheduleAt(simTime()+t0, beep);
}

void WebServer::handleMessage(cMessage *msg)
{
    if( msg->isSelfMessage() ){
        int sizePkt = uniform(3,75); // da sistemare il RNG

        // generation of a new packet
        UserPacket *msg = new UserPacket(NULL, 0);
        msg->setBitLength(sizePkt*8);
        msg->setStart_time(simTime());
        send(msg, "outData_p");

        // scheduling next packet generation
        simtime_t t1 = exponential(1/lambda,0)/1000;
        scheduleAt(simTime()+t1, beep);
    }
}
