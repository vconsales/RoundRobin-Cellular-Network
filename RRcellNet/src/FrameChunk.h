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

#ifndef FRAMECHUNK_H_
#define FRAMECHUNK_H_

#include <omnetpp/cmessage.h>
#include <omnetpp.h>

using namespace omnetpp;

class FrameChunk: public omnetpp::cMessage {
private:
    unsigned short frame_offset;
    unsigned short RB_size;
    unsigned short RB_count;
    cPacketQueue packet_list;

public:
    FrameChunk(unsigned short frame_offset, unsigned short RB_size);
    void setRBCount(unsigned short RB_count);
    unsigned short getRBCount();

    void insertPacket(cPacket *pkt);
    cPacket *extractPacket();
    unsigned int packetCount();
    unsigned long int totalCarriedBits();
    virtual ~FrameChunk();

    const char* getDisplayString() const;
};

#endif /* FRAMECHUNK_H_ */
