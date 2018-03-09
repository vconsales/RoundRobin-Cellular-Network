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

module Antenna
{
    parameters:
        int nUsers;
        @display("bgb=463,263");
    gates:
        input inCQI_p[nUsers];
        output outData_p[nUsers];
    submodules:
        webServer[nUsers]: WebServer {
            @display("p=72,119");
        }
        queue[nUsers]: FIFOQueue {
            @display("p=235,119");
        }
        scheduler: Scheduler {
            nUsers = nUsers;
            @display("p=372,119");
        }
    connections:
        for i=0..nUsers-1 {
            webServer[i].outData_p --> queue[i].inData_p;
            inCQI_p[i] --> scheduler.inCQISched_p[i];
            queue[i].outData_p --> scheduler.inDataSched_p[i];
            scheduler.outDataSched_p[i] -->  outData_p[i]; 
        }

}
