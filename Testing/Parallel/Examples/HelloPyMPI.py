# Simple example to demonstrate use of Profiler
from mpi4py import MPI
from profiler import Profiler
import time
import sys
import ABaTe as abate

testname = "HelloPyMPI"
mpicommobj = MPI.COMM_WORLD
myrank = mpicommobj.Get_rank()
numproc = mpicommobj.Get_size()

myprofiler = Profiler(mpicommobj)

myprofiler.starttimer()
myprofiler.starttimer("Setup")

if myrank == 0:
    print(numproc, " processors ready.")

myprofiler.endtimer("Setup")

for j in range(4):

    myprofiler.starttimer("Hello")

    for i in range(numproc):
        if i == myrank:
            print("(", myrank, ") Hello Python MPI Comm World!")
            mpicommobj.Barrier()

    mpicommobj.Barrier()

    # introduce intentional time imbalance for > 3 procs
    if myrank == 3:
        time.sleep(1)

    myprofiler.endtimer("Hello")

# myprofiler.endtimer("ErrorTest")
myprofiler.endtimer()

if myrank == 0:
    print("All done.")
    print("Rank 0 Profile:")
    myprofiler.writeserialprofile()
    # Could use this to write it to file:
    # myprofiler.WriteSerialProfile(testname+"_rank0_"+str(numproc))
    print("----- Parallel Profile -----")

# this is a collective call to give parallel stats
# stdout if optional filename argument is missing
myprofiler.writeparallelprofile()
# Profiler can help you form a filename if you want
# to write the parallel profile to file:
# parallelProfileFileName = myprofiler.ParallelProfileFileName(testname)
# myprofiler.WriteParallelProfile(parallelProfileFileName)

if myrank == 0:
    numargs = len(sys.argv)
    outfilename = ""

    if numargs > 1:
        outfilename = sys.argv[1]

    testresult = {testname: "Pass"}
    abate.updatetestresults(testresult, outfilename)
