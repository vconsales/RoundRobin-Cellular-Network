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
simple Scheduler
{
    parameters:
        int nSlotsFrame;
        double timeSlotPeriod @unit(ms);
        double timeFramePeriod @unit(ms);
        int nUsers;
    gates:
        input  inCQISched_p[nUsers];
        input  inDataSched_p[nUsers]; //la coda invia i pacchetti qui
        output outDataSched_p[nUsers]; //qui escono gli RB diretti all'utente
}
