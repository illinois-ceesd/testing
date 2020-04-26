from mpi4py import MPI
from time import perf_counter as GetTime
import numpy as NUMPY
import sys


class Profiler:
    def ResetTime(self):
        self.t0 = GetTime()

    def SetBarrier(self, inOption):
        self.timerBarrier = inOption

    def SetMPICommunicator(self, inMPICommunicator):
        self.mpiCommObj = inMPICommunicator
        self.myRank = self.mpiCommObj.Get_rank()
        self.numProc = self.mpiCommObj.Get_size()

    def __init__(self, inMPICommunicator):
        self.SetMPICommunicator(inMPICommunicator)
        self.ResetTime()
        self.openSections = []
        self.sectionTimes = []
        self.timerBarrier = True

    def StartTimer(self, sectionName="MAIN"):

        if sectionName == "MAIN":
            self.ResetTime()

        numOpenSections = len(self.openSections)

        openSection = [numOpenSections, sectionName, 1, 0, 0]

        if self.timerBarrier:
            self.mpiCommObj.Barrier()

        openSection[3] = GetTime()

        self.openSections.append(openSection)

    def EndTimer(self, sectionName="MAIN"):
        numOpenSections = len(self.openSections)

        if numOpenSections <= 0:
            print(
                "Error: EndTimer(",
                sectionName,
                ") called with no matching StartTimer.",
            )
            1 / 0

        sectionTime = GetTime()

        if self.timerBarrier:
            self.mpiCommObj.Barrier()

        openSectionIndex = numOpenSections - 1

        if sectionName != self.openSections[openSectionIndex][1]:
            print(
                "SectionName: Expected(",
                self.openSections[openSectionIndex][1],
                ")",
                ", Got(",
                sectionName,
                ")",
            )
            1 / 0

        openSection = self.openSections.pop()
        sectionTime = sectionTime - openSection[3]
        openSection[3] = sectionTime
        openSectionIndex = openSectionIndex - 1

        # Update parent's sub-timers
        if openSectionIndex >= 0:
            self.openSections[openSectionIndex][4] += sectionTime

        # Update section if it exists
        numSections = len(self.sectionTimes)
        match = False

        for i in range(numSections):
            if self.sectionTimes[i][1] == sectionName:
                existingSection = self.sectionTimes[i]
                existingSection[2] += 1
                existingSection[3] += sectionTime
                existingSection[4] += openSection[4]
                match = True
                break

        # Create new section if it didn't exist
        if not match:
            self.sectionTimes.append(openSection)

    def WriteSerialProfile(self, fileName=""):

        # copy the timers to avoid destructing the list when printing
        sectionTimers = list(self.sectionTimes)

        numSections = len(sectionTimers)
        numCurrentSections = numSections
        minLevel = 0

        profileFile = sys.stdout

        if fileName != "":
            profileFile = open(fileName, "w")

        if numCurrentSections > 0:
            print(
                "# SectionName   NumCalls  TotalTime   ChildTime",
                file=profileFile,
            )

        while numCurrentSections > 0:
            match = False
            for i in range(numCurrentSections):
                if sectionTimers[i][0] == minLevel:
                    sectionTimer = sectionTimers.pop()
                    # print out SectionName NumCalls TotalTime ChildTime
                    print(
                        sectionTimer[1],
                        sectionTimer[2],
                        sectionTimer[3],
                        sectionTimer[4],
                        file=profileFile,
                    )
                    match = True
                    break

            if match is False:
                minLevel += 1

            numCurrentSections = len(sectionTimers)

        if fileName != "":
            profileFile.close()

    # WriteParallelProfile is a collective call, must be called on all procs
    def WriteParallelProfile(self, fileName=""):

        sectionTimers = list(self.sectionTimes)

        numSections = len(sectionTimers)
        myNumSections = NUMPY.zeros(1, dtype=int)
        myCheck = NUMPY.zeros(1, dtype=int)

        self.mpiCommObj.Barrier()
        numProc = self.mpiCommObj.Get_size()

        if self.myRank == 0:
            myNumSections[0] = numSections

        self.mpiCommObj.Bcast(myNumSections, root=0)

        if numSections == myNumSections[0]:
            myNumSections[0] = 0
        else:
            myNumSections[0] = 1
            print(
                "(", self.myRank, "): ", numSections, " != ", myNumSections[0]
            )
            1 / 0

        self.mpiCommObj.Reduce(myNumSections, myCheck, MPI.MAX, 0)

        if myCheck > 0:
            print(
                "ReduceTimers:Error: Disparate number of sections ",
                "across processors.",
            )
            1 / 0

        mySectionTimes = NUMPY.zeros(numSections, dtype="float")
        minTimes = NUMPY.zeros(numSections, dtype="float")
        maxTimes = NUMPY.zeros(numSections, dtype="float")
        sumTimes = NUMPY.zeros(numSections, dtype="float")

        for i in range(numSections):
            mySectionTimes[i] = sectionTimers[i][3]

        self.mpiCommObj.Reduce(mySectionTimes, minTimes, MPI.MIN, 0)
        self.mpiCommObj.Reduce(mySectionTimes, maxTimes, MPI.MAX, 0)
        self.mpiCommObj.Reduce(mySectionTimes, sumTimes, MPI.SUM, 0)

        if self.myRank == 0:

            profileFile = sys.stdout
            if fileName != "":
                profileFile = open(fileName, "w")
            print("# NumProcs: ", numProc, file=profileFile)
            print(
                "# SectionName   MinTime   MaxTime   MeanTime",
                file=profileFile,
            )
            for i in range(numSections):
                sectionTime = sectionTimers[i]
                print(
                    sectionTime[1],
                    minTimes[i],
                    maxTimes[i],
                    sumTimes[i] / float(self.numProc),
                    file=profileFile,
                )

            if fileName != "":
                profileFile.close()

        self.mpiCommObj.Barrier()

    def ParallelProfileFileName(self, rootName=""):

        myRootName = rootName
        if myRootName == "":
            myRootName = "Profiler"

        numProc = self.mpiCommObj.Get_size()

        profileFileName = myRootName + "_ParTimes_" + str(numProc)

        return profileFileName
