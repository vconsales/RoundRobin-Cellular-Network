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
    // RNGs parameters
    lambda = par("lambda");
    size_uniform_a = par("size_uniform_a");
    size_uniform_b = par("size_uniform_b");
    constantRate = par("constantRate");

    beep = new cMessage("beep");

    // start packet schedule
    nextPacketSchedule();
}

void WebServer::handleMessage(cMessage *msg)
{
    if( msg->isSelfMessage() ){
        int sizePkt = uniform(size_uniform_a, size_uniform_b, RNG_UNI_PACKETSIZE_INDEX);

        // generation of a new packet
        UserPacket *msg = new UserPacket(NULL, 0);
        msg->setByteLength(sizePkt);
        msg->setStart_time(simTime());  // this will be used to compute the response time
        send(msg, "outData_p");

        // scheduling next packet generation
        nextPacketSchedule();
    }
}

void WebServer::nextPacketSchedule() {
    simtime_t interarrival_time;

    if( constantRate )
        interarrival_time =1/(lambda*1000);
    else
        interarrival_time = exponential((1/lambda), RNG_EXP_INTERARRIVAL_INDEX)/1000;

    scheduleAt(simTime() + interarrival_time, beep);
}

WebServer::~WebServer() {
    this->cancelAndDelete(beep);
}
