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

#ifndef __RRCELLNET_WEBSERVER_H_
#define __RRCELLNET_WEBSERVER_H_

#include <omnetpp.h>

using namespace omnetpp;

/**
 * TODO - Generated class
 */
class WebServer : public cSimpleModule
{
private:
    cMessage *beep;

    //RNGs indexes
    const short RNG_EXP_INTERARRIVAL_INDEX = 0;
    const short RNG_UNI_PACKETSIZE_INDEX = 1;

    // packets rate
    double lambda;

    // uniform distribution packet size parameters
    unsigned int size_uniform_a;
    unsigned int size_uniform_b;

    // used to validate the model
    bool fixedRate;
protected:
    virtual void initialize();
    virtual void handleMessage(cMessage *msg);
    void nextPacketSchedule();
    ~WebServer();
};

#endif
