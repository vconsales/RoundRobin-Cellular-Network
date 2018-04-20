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

package rrcellnet;

//
// TODO auto-generated module
//
simple MobileStation
{
    parameters:
        int nFrameSlots;
        double timeFramePeriod @unit(ms);

		// used for validation tests
		bool validationCQI;
		int fixedCQI;
        // choice between binomial and uniform
        bool isBinomial;

        // configurable parameters for binomial distribution
        int cqi_binomial_n;
		double cqi_binomial_p;

        // throughtput total mean value (scalar type)
        // TODO: we must discard the warm-up period, NOT IMPLEMENTED FOR NOW
        @signal[throughputBits](type=double);
        @statistic[throughputBits](source "throughputBits";record=last;);

        // slotted mean throughput (vector type, x=time, y=bits/s)
        @signal[slottedThroughputBits](type=double);
        @statistic[slottedThroughputBits](source "slottedThroughputBits";record=vector;);

        // mean response time (vector type, x=time, y=time + mean)
        @signal[responseTime](type=simtime_t);
        @statistic[responseTime](source "responseTime";record=vector,mean;);

    gates:
        input  inData_p;
        output outCQI_p;
}
