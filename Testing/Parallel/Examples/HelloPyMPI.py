# Simple example to demonstrate use of Profiler
from mpi4py import MPI
from profiler import Profiler
import time
import sys
import juketest

testName = "HelloPyMPI"
mpiCommObj = MPI.COMM_WORLD
myRank = mpiCommObj.Get_rank()
numProc = mpiCommObj.Get_size()

myProfiler = Profiler(mpiCommObj)

myProfiler.StartTimer()
myProfiler.StartTimer("Setup")

if myRank == 0:
    print(numProc, " processors ready.")

myProfiler.EndTimer("Setup")

for j in range(4):

    myProfiler.StartTimer("Hello")

    for i in range(numProc):
        if i == myRank:
            print("(", myRank, ") Hello Python MPI Comm World!")
            mpiCommObj.Barrier()

    mpiCommObj.Barrier()

    # introduce intentional time imbalance for > 3 procs
    if myRank == 3:
        time.sleep(1)

    myProfiler.EndTimer("Hello")

# myProfiler.EndTimer("ErrorTest")
myProfiler.EndTimer()

if myRank == 0:
    print("All done.")
    print("Rank 0 Profile:")
    myProfiler.WriteSerialProfile()
    # Could use this to write it to file:
    # myProfiler.WriteSerialProfile(testName+"_rank0_"+str(numProc))
    print("----- Parallel Profile -----")

# this is a collective call to give parallel stats
# stdout if optional filename argument is missing
myProfiler.WriteParallelProfile()
# Profiler can help you form a filename if you want
# to write the parallel profile to file:
# parallelProfileFileName = myProfiler.ParallelProfileFileName(testName)
# myProfiler.WriteParallelProfile(parallelProfileFileName)

if myRank == 0:
    numArgs = len(sys.argv)
    outFileName = ""

    if numArgs > 1:
        outFileName = sys.argv[1]

    testResult = {testName: "Pass"}
    juketest.UpdateTestResults(testResult, outFileName)
