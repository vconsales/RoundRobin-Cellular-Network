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

#ifndef __RRCELLNET_FIFOQUEUE_H_
#define __RRCELLNET_FIFOQUEUE_H_

#include <omnetpp.h>
#include <omnetpp/cpacketqueue.h>
using namespace omnetpp;

/**
 * TODO - Generated class
 */
class FIFOQueue : public cSimpleModule
{
  private:
    cGate *inData_p;
    cGate *outData_p;
  protected:
    cPacketQueue queue;
    virtual void initialize();
    virtual void handleMessage(cMessage *msg);
  public:
    cPacket* getPacket();
    bool popFront();
};

#endif
