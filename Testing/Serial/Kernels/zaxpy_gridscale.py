import sys
import numpy as np
import numpy.linalg as la
import pyopencl as cl
import pyopencl.array
import pyopencl.clrandom

from mpi4py import MPI
import loopy as lp
from profiler import Profiler
import pyjuke as jk
import ABaTe as abate

mpicommobj = MPI.COMM_WORLD
myprofiler = Profiler(mpicommobj)
testname = "zaxpy_gridscale"


def main(datapath):

    global myprofiler

    fn = datapath + "zaxpy3.f90"

    with open(fn, "r") as inf:
        source = inf.read()

    myprofiler.starttimer("LoopyParse")
    #    dgemm, = lp.parse_transformed_fortran(source, filename=fn)
    (zaxpy3,) = lp.parse_fortran(source, filename=fn)
    zaxpy3 = lp.set_options(zaxpy3, write_code=True)
    myprofiler.endtimer("LoopyParse")

    gridsize = 16
    while gridsize < 1024:

        print("Processing GridSize = ", gridsize)

        myprofiler.starttimer("CLContext")
        ctx = cl.create_some_context(False)
        queue = cl.CommandQueue(ctx)
        myprofiler.endtimer("CLContext")

        isize = gridsize
        jsize = gridsize
        ksize = gridsize

        imin = 1
        jmin = 1
        kmin = 1

        imax = isize
        jmax = jsize
        kmax = ksize

        gridsizestr = str(gridsize)
        initname = "CLInit-" + gridsizestr

        myprofiler.starttimer(initname)
        x = cl.array.empty(
            queue, (isize, jsize, ksize), dtype=np.float64, order="F"
        )
        y = cl.array.empty(
            queue, (isize, jsize, ksize), dtype=np.float64, order="F"
        )
        z = cl.array.empty(
            queue, (isize, jsize, ksize), dtype=np.float64, order="F"
        )

        cl.clrandom.fill_rand(x)
        cl.clrandom.fill_rand(y)
        a = gridsize
        z_ref = a * x.get() + y.get()

        myprofiler.endtimer(initname)

        zaxpyname = "zaxpy3-" + gridsizestr
        myprofiler.starttimer(zaxpyname)
        zaxpy3(
            queue,
            imin=imin,
            imax=imax,
            jmin=jmin,
            jmax=jmax,
            kmin=kmin,
            kmax=kmax,
            a=a,
            x=x,
            y=y,
            z=z,
        )
        z.get()
        myprofiler.endtimer(zaxpyname)

        checkname = "CheckResult-" + gridsizestr
        myprofiler.starttimer(checkname)
        errnorm = la.norm(z_ref - z.get()) / la.norm(z_ref)
        assert errnorm < 1e-10
        myprofiler.endtimer(checkname)

        gridsize = 2 * gridsize


if __name__ == "__main__":

    myprofiler.starttimer()

    datapath = "./"
    datapath = jk.sourcepath + "/kernels/"
    print("DataPath: ", datapath)

    myprofiler.starttimer("runtest")
    main(datapath)
    myprofiler.endtimer("runtest")

    numargs = len(sys.argv)
    outfilename = ""

    if numargs > 1:
        outfilename = sys.argv[1]

    testresult = {testname: "Pass"}
    abate.updatetestresults(testresult, outfilename)

    myprofiler.endtimer()
    myprofiler.writeserialprofile("zaxpy_gridscale_timing")
