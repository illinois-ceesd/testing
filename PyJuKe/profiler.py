from mpi4py import MPI
from time import perf_counter as gettime
import numpy
import sys


class Profiler:
    def resettime(self):
        self.t0 = gettime()

    def setbarrier(self, inoption):
        self.timerbarrier = inoption

    def setmpicommunicator(self, inmpicommunicator):
        self.mpicommobj = inmpicommunicator
        self.myrank = self.mpicommobj.Get_rank()
        self.numproc = self.mpicommobj.Get_size()

    def __init__(self, inmpicommunicator):
        self.setmpicommunicator(inmpicommunicator)
        self.resettime()
        self.opensections = []
        self.sectiontimes = []
        self.timerbarrier = True

    def starttimer(self, sectionname="MAIN"):

        if sectionname == "MAIN":
            self.resettime()

        numopensections = len(self.opensections)

        opensection = [numopensections, sectionname, 1, 0, 0]

        if self.timerbarrier:
            self.mpicommobj.Barrier()

        opensection[3] = gettime()

        self.opensections.append(opensection)

    def endtimer(self, sectionname="MAIN"):
        numopensections = len(self.opensections)

        if numopensections <= 0:
            print(
                "Error: EndTimer(",
                sectionname,
                ") called with no matching StartTimer.",
            )
            1 / 0

        sectiontime = gettime()

        if self.timerbarrier:
            self.mpicommobj.Barrier()

        opensectionindex = numopensections - 1

        if sectionname != self.opensections[opensectionindex][1]:
            print(
                "SectionName: Expected(",
                self.opensections[opensectionindex][1],
                ")",
                ", Got(",
                sectionname,
                ")",
            )
            1 / 0

        opensection = self.opensections.pop()
        sectiontime = sectiontime - opensection[3]
        opensection[3] = sectiontime
        opensectionindex = opensectionindex - 1

        # Update parent's sub-timers
        if opensectionindex >= 0:
            self.opensections[opensectionindex][4] += sectiontime

        # Update section if it exists
        numsections = len(self.sectiontimes)
        match = False

        for i in range(numsections):
            if self.sectiontimes[i][1] == sectionname:
                existingsection = self.sectiontimes[i]
                existingsection[2] += 1
                existingsection[3] += sectiontime
                existingsection[4] += opensection[4]
                match = True
                break

        # Create new section if it didn't exist
        if not match:
            self.sectiontimes.append(opensection)

    def writeserialprofile(self, filename=""):

        # copy the timers to avoid destructing the list when printing
        sectiontimers = list(self.sectiontimes)

        numsections = len(sectiontimers)
        numcurrentsections = numsections
        minlevel = 0

        profilefile = sys.stdout

        if filename != "":
            profilefile = open(filename, "w")

        if numcurrentsections > 0:
            print(
                "# SectionName   NumCalls  TotalTime   ChildTime",
                file=profilefile,
            )

        while numcurrentsections > 0:
            match = False
            for i in range(numcurrentsections):
                if sectiontimers[i][0] == minlevel:
                    sectiontimer = sectiontimers.pop()
                    # print out SectionName NumCalls TotalTime ChildTime
                    print(
                        sectiontimer[1],
                        sectiontimer[2],
                        sectiontimer[3],
                        sectiontimer[4],
                        file=profilefile,
                    )
                    match = True
                    break

            if match is False:
                minlevel += 1

            numcurrentsections = len(sectiontimers)

        if filename != "":
            profilefile.close()

    # WriteParallelProfile is a collective call, must be called on all procs
    def writeparallelprofile(self, filename=""):

        sectiontimers = list(self.sectiontimes)

        numsections = len(sectiontimers)
        mynumsections = numpy.zeros(1, dtype=int)
        mycheck = numpy.zeros(1, dtype=int)

        self.mpicommobj.Barrier()
        numproc = self.mpicommobj.Get_size()

        if self.myrank == 0:
            mynumsections[0] = numsections

        self.mpicommobj.Bcast(mynumsections, root=0)

        if numsections == mynumsections[0]:
            mynumsections[0] = 0
        else:
            mynumsections[0] = 1
            print(
                "(", self.myrank, "): ", numsections, " != ", mynumsections[0]
            )
            1 / 0

        self.mpicommobj.Reduce(mynumsections, mycheck, MPI.MAX, 0)

        if mycheck > 0:
            print(
                "ReduceTimers:Error: Disparate number of sections ",
                "across processors.",
            )
            1 / 0

        mysectiontimes = numpy.zeros(numsections, dtype="float")
        mintimes = numpy.zeros(numsections, dtype="float")
        maxtimes = numpy.zeros(numsections, dtype="float")
        sumtimes = numpy.zeros(numsections, dtype="float")

        for i in range(numsections):
            mysectiontimes[i] = sectiontimers[i][3]

        self.mpicommobj.Reduce(mysectiontimes, mintimes, MPI.MIN, 0)
        self.mpicommobj.Reduce(mysectiontimes, maxtimes, MPI.MAX, 0)
        self.mpicommobj.Reduce(mysectiontimes, sumtimes, MPI.SUM, 0)

        if self.myrank == 0:

            profilefile = sys.stdout
            if filename != "":
                profilefile = open(filename, "w")
            print("# NumProcs: ", numproc, file=profilefile)
            print(
                "# SectionName   MinTime   MaxTime   MeanTime",
                file=profilefile,
            )
            for i in range(numsections):
                sectiontime = sectiontimers[i]
                print(
                    sectiontime[1],
                    mintimes[i],
                    maxtimes[i],
                    sumtimes[i] / float(self.numproc),
                    file=profilefile,
                )

            if filename != "":
                profilefile.close()

        self.mpicommobj.Barrier()

    def makeparallelfilename(self, rootname=""):

        myrootname = rootname
        if myrootname == "":
            myrootname = "Profiler"

        numproc = self.mpicommobj.Get_size()

        profilefilename = myrootname + "_ParTimes_" + str(numproc)

        return profilefilename
