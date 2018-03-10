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

#ifndef RESOURCEBLOCK_H_
#define RESOURCEBLOCK_H_

#include <omnetpp.h>
using namespace omnetpp;

const int CQI_B[16] = {0,3,3,3,6,11,15,20,25,36,50,63,72,80,93,93};

class ResourceBlock : public cMessage{
private:
    int idPkt;
    int size;
public:
    // primo parametro: id del pacchetto originario
    // secondo parametro: CQI
    ResourceBlock(int idPkt, int CQI);
    int getIdPacket();
    int getSizeByte();
    virtual ~ResourceBlock();
};

#endif /* RESOURCEBLOCK_H_ */
