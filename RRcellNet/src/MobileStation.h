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

#ifndef __RRCELLNET_MOBILESTATION_H_
#define __RRCELLNET_MOBILESTATION_H_

#include <omnetpp.h>

using namespace omnetpp;

/**
 * TODO - Generated class
 */
class MobileStation : public cSimpleModule
{
private:
    static unsigned int idUser_counter;
    int idUser;
    int nFrameSlots;
    simtime_t timeFramePeriod;
    cMessage *beepMS;
    cGate *inData_p;
    cGate *outCQI_p;

// statitische
    uint64_t receivedBytes;
    uint64_t receivedPacket;

    simsignal_t throughputBits_s;
    simsignal_t slottedThroughputBits_s;
    simsignal_t responseTime_s;
  protected:
    virtual void initialize();
    virtual void handleMessage(cMessage *msg);
};

#endif
