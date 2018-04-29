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

#include "Scheduler.h"
#include <assert.h>
#include "FrameChunk.h"

#include <vector>
#include <algorithm>
#include <functional>

Define_Module(Scheduler);

void Scheduler::initialize()
{
    nUsers = par("nUsers");
    nFrameSlots = par("nFrameSlots");
    timeFramePeriod = par("timeFramePeriod");

    // scheduler choosen policy for frame filling
    bestCQIScheduler = par("bestCQIScheduler");

    // we start from the first user, so id 0 user
    currentUser = 0;
    beepSched = new cMessage("beepScheduler");

    char buf[100];
    for(unsigned int i=0; i<nUsers; i++){
        vec_outData.push_back(gate("outDataSched_p",i));
        EV << "vec_outData[i]: "<< vec_outData[i] << endl;

        sprintf(buf,"CellularNetwork.antenna.queue[%d]",i);
        cModule *m = getModuleByPath(buf);
        FIFOQueue *f = check_and_cast<FIFOQueue *>(m);
        vec_q.push_back(f);
    }

    //TODO: RISOLVERE PROBLEMA DELLA RICEZIONE DI TUTTI I CQI PRIMA DI COMPORRE IL FRAME
    scheduleAt(simTime()+0.0001f, beepSched);
}

int integerRoundDivision(const int n, const int d)
{
    return ((n < 0) ^ (d < 0)) ? ((n - d/2)/d) : ((n + d/2)/d);
}

void Scheduler::handleMessage(cMessage *msg)
{
    if( msg->isSelfMessage() ){
        sendRBs();

        // see you at the next timeslot...
        scheduleAt(simTime()+(timeFramePeriod/1000), beepSched);

    } else if( strcmp(msg->getName(),"cqiMSG") == 0 ){
        updateCQIs(msg);
    }
}

Scheduler::~Scheduler(){
  this->cancelAndDelete(beepSched);
}

int Scheduler::nextUser(){
    return currentUser=(currentUser+1)%nUsers;
}

void Scheduler::updateCQIs(cMessage *msg)
{
    cArray parList = msg->getParList();
   // EV <<"parList size:"<< parList.size() << endl;
    int idUser = ((cMsgPar*)parList[0])->longValue();
    int CQI = ((cMsgPar*)parList[1])->longValue();
    rrUserStruct temp(idUser,CQI);
    usersVector.push_back(temp);

    assert(CQI != 0);
    EV << "updateCQIs: idUser=" << idUser << " CQI=" << CQI << endl;
    delete msg;
}

void Scheduler::scheduleUsers() {
    std::function<bool(rrUserStruct,rrUserStruct)> lambda_fairScheduler =
            [this] (rrUserStruct first, rrUserStruct second) {
                // currentUser must be always on top
                if(first.userId == this->currentUser)
                    return true;
                else if(second.userId == this->currentUser)
                    return false;

                // we want to order users from currentUser to the end and then WRAP
                // example: currentUser=2 nUsers=6 => 2 3 4 5 0 1
                if(first.userId > this->currentUser && second.userId < this->currentUser)
                    return true;
                else if(first.userId < this->currentUser && second.userId > this->currentUser)
                    return false;

                // else compare IDs
                return first.userId < second.userId;
            };

    std::function<bool(rrUserStruct,rrUserStruct)> lambda_bestCQIScheduler =
            [this,lambda_fairScheduler] (rrUserStruct first, rrUserStruct second) {
                // currentUser must be always on top
                if(first.userId == this->currentUser)
                    return true;
                else if(second.userId == this->currentUser)
                    return false;

                // else compare CQIs
                if(first.receivedCQI > second.receivedCQI)
                    return true;
                else if( first.receivedCQI < second.receivedCQI)
                    return false;

                // what if CQIs are equal?
                // just use the fair scheduling (:
                return lambda_fairScheduler(first, second);
            };


    // if chosen, sort users using the best-CQI policy
    if(bestCQIScheduler)
    {
        std::sort(usersVector.begin(), usersVector.end(), lambda_bestCQIScheduler);
        EV << "ORDERING BY BEST CQI" << endl;
    }
    else    // otherwise we will user a fair scheduling
    {
        std::sort(usersVector.begin(), usersVector.end(), lambda_fairScheduler);
        EV << "ORDERING BY FAIR CQI" << endl;
    }
}

void Scheduler::sendRBs()
{
    EV << "scheduler self3" << endl;

    // this algorithm will directly cycle users in usersVector list.
    // the order of served clients is defined by this method by the chosen policy
    scheduleUsers();

    // the user we are working on is currentUser, but we need to cycle the
    // other users to eventually fill the remaining frame space, without
    // touching currentUser member
    unsigned int nowServingIndex = 0;

    // we need to fill all the RBs
    int freeRBs = nFrameSlots;
    while(freeRBs && nowServingIndex < nUsers)
    {
        // depending on the (user related) CQI and the RB count
        // we can compute the total available space in frame
        int curID = usersVector[nowServingIndex].userId;
        int curCQI = usersVector[nowServingIndex].receivedCQI;

        EV << "Scheduler: moving to next user id=" << curID << " cqi=" << curCQI << endl;

        // ## !! this is an assertion to check that CQI is sent by all the users
        assert(curCQI!=0);
        // ##

        int RBbytes = CQI_B[curCQI];
        int freeFrameBytes = RBbytes*freeRBs;

        // we will send a FrameChunk to the user
        FrameChunk *fchunk = new FrameChunk(curID, RBbytes);

        // fetch packet by packet from currentUser queue
        for(cPacket *pkt = vec_q[curID]->getPacket();
                pkt != nullptr; pkt = vec_q[curID]->getPacket())
        {
            int pktSize = pkt->getByteLength();
            EV << "Scheduler: pkt size=" << pktSize << " freeFrameBytes=" << freeFrameBytes
                    << " RBbytes=" << RBbytes <<endl;
            if(pktSize <= freeFrameBytes)
            {
                freeFrameBytes -= pktSize;
                // remove the packet from the queue and push it into the FrameChunk
                fchunk->insertPacket(vec_q[curID]->popFront());
            }
            else // not schedulable
                break;  // we must stop the schedulation because of the FIFO rule
        }

        // if we are here the current user queue is empty.
        // at this point we have just computed the frame allocation in terms of bytes,
        // we must convert it in terms of allocated RBs
        int allocatedFrameSpace = RBbytes*freeRBs - freeFrameBytes;
        assert(allocatedFrameSpace >= 0);

        // we use the round() function because a partially allocated RB must be considered
        // as allocated and must not be used by the next user
        assert(RBbytes!=0);
        int allocatedRbs = integerRoundDivision(allocatedFrameSpace, RBbytes);
        EV << "Scheduler: allocatedRB = " << allocatedRbs << endl;

        freeRBs -= allocatedRbs;

        // now we can send the FrameChunk to the current user if it contains at least
        // one packet (the condition is just for a visual debugging purpose)
        if(fchunk->packetCount() != 0)
            send(fchunk, vec_outData[curID]);
        else
            delete fchunk;

        assert(freeRBs >= 0);

        // we need to fill the frame, so we will fetch the packets from the next scheduled user
        nowServingIndex++;
    }

    // user vector must be cleared before the next sendRBs method call
    usersVector.clear();

    // the next frame composing will work on the next user, following the Round Robin policy.
    nextUser();
}
