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

#include "MobileStation.h"
#include "FrameChunk.h"
#include "UserPacket_m.h"

Define_Module(MobileStation);

unsigned int MobileStation::idUser_counter = 0;

void MobileStation::initialize()
{
    idUser = idUser_counter++;

    // beep, parameters and gates
    beepMS = new cMessage("beepMS");
    nFrameSlots = par("nFrameSlots");
    timeFramePeriod = par("timeFramePeriod");
    inData_p = gate("inData_p");
    outCQI_p = gate("outCQI_p");

    // statistic members
    totalReceivedBits = 0;
    lastSlotReceivedBits = 0;

    // CQI RNG parameters fetch
    isBinomial = par("isBinomial");
    cqi_binomial_n = par("cqi_binomial_n");
    cqi_binomial_p = par("cqi_binomial_p");

    // scalar statistics
    throughputBits_s = registerSignal("throughputBits");

    // vector statistics
    slottedThroughputBits_s = registerSignal("slottedThroughputBits");
    responseTime_s = registerSignal("responseTime");

    scheduleAt(simTime(), beepMS);
}

void MobileStation::finish() {
    // scalar mean throughput
    emit(throughputBits_s, totalReceivedBits/(simTime() - getSimulation()->getWarmupPeriod()));
}

void MobileStation::sendCQI() {
    EV << "MobileStation (id=" << idUser << "): generating packet" << endl;

    cMessage *cqiMSG = new cMessage("cqiMSG");

    cMsgPar *idUserPar = new cMsgPar("idUser");
    idUserPar->setLongValue(idUser);
    cMsgPar *cqiPar = new cMsgPar("CQI");

    int randCQI;
    if(isBinomial)
        // we add 1 because binomial is non null between 0 and cqi_binomial_n.
        // we must take into account this variation when choosing cqi_binomial_n
        randCQI = binomial(cqi_binomial_n, cqi_binomial_p, RNG_CQI_INDEX) + 1;
    else
        randCQI = intuniform(CQI_UNIFORM_A, CQI_UNIFORM_B, RNG_CQI_INDEX);
    cqiPar->setLongValue(randCQI);

    cqiMSG->addPar(idUserPar);
    cqiMSG->addPar(cqiPar);
    send(cqiMSG,outCQI_p);

    // slotted throughput (of the previous slot)
    emit(slottedThroughputBits_s, lastSlotReceivedBits/(timeFramePeriod/1000));

    // we expect to receive a FrameChunk in this slot, so if we
    // receive a FrameChunk this value will be updated
    lastSlotReceivedBits = 0;

    scheduleAt(simTime()+timeFramePeriod/1000, beepMS);
}

void MobileStation::handleFrameChunk(cMessage *msg) {
    //  EV << "pkt received " << msg->getName() << endl;
    FrameChunk *fchunk = check_and_cast<FrameChunk *>(msg);

    // we set the received packet size related to the current slot
    lastSlotReceivedBits = fchunk->totalCarriedBits();

    totalReceivedBits += fchunk->totalCarriedBits();
    EV << "Received " <<  fchunk->totalCarriedBits() << " bits, " << fchunk->totalCarriedBits()/1024 << " bytes" << endl;

    // response time data
    simtime_t end_time = simTime();
    for(cPacket *pkt = fchunk->extractPacket(); pkt!=nullptr; pkt = fchunk->extractPacket())
    {
        UserPacket *user_pkt = check_and_cast<UserPacket*>(pkt);
        emit(responseTime_s, end_time - user_pkt->getStart_time());
        EV << "packet received bytes=" << pkt->getByteLength() << " bits=" << pkt->getBitLength() << endl;

        delete user_pkt;
    }

    delete fchunk;
}

void MobileStation::handleMessage(cMessage *msg)
{
    if(msg->isSelfMessage())
        sendCQI();
    else
        handleFrameChunk(msg);
}

MobileStation::~MobileStation()
{
    //TODO: This is not so correct.
    idUser_counter--;
}
