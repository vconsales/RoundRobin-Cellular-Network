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

#include "FrameChunk.h"

FrameChunk::FrameChunk(unsigned short frame_offset, unsigned short RB_size)
: cMessage("FrameChunk"), frame_offset(frame_offset), RB_size(RB_size), packet_list() {

}

void FrameChunk::insertPacket(cPacket *pkt) {
    if(pkt==nullptr)
        return;
    packet_list.insert(pkt);
}

cPacket* FrameChunk::extractPacket() {
    if(packet_list.isEmpty())
        return nullptr;
    return packet_list.pop();
}

unsigned int FrameChunk::packetCount() {
    return packet_list.getLength();
}

unsigned long int FrameChunk::totalCarriedBits() {
    return packet_list.getBitLength();
}

FrameChunk::~FrameChunk() {
    cPacket *tmp;
    while((tmp=extractPacket())!=nullptr)
        delete tmp;
}

const char* FrameChunk::getDisplayString() const
{
    return "b=60,10,rect,lightblue,black,1";
}
