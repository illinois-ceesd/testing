import sys
# from mpi4py import pympi
import numpy as np
import numpy.linalg as la
import pyopencl as cl
import pyopencl.array
import pyopencl.clrandom

from mpi4py import MPI
import loopy as lp
from profiler import Profiler
#import pyjuke

mpicommobj = MPI.COMM_WORLD
myprofiler = Profiler(mpicommobj)


def main(datapath):

    global myprofiler

    fn = datapath+"zaxpy3.f90"

    with open(fn, "r") as inf:
        source = inf.read()

    myprofiler.starttimer("LoopyParse")
#    dgemm, = lp.parse_transformed_fortran(source, filename=fn)
    zaxpy3, = lp.parse_fortran(source, filename=fn)
    zaxpy3 = lp.set_options(zaxpy3, write_code=True)
    myprofiler.endtimer("LoopyParse")

    myprofiler.starttimer("CLContext")
#    ctx = cl.create_some_context(False, pyjuke.cl_context_answers)
    ctx = cl.create_some_context(False)
    queue = cl.CommandQueue(ctx)
    myprofiler.endtimer("CLContext")

    isize = 128
    jsize = 128
    ksize = 128

    imin = 1
    jmin = 1
    kmin = 1

    imax = isize
    jmax = jsize
    kmax = ksize

    myprofiler.starttimer("CLInit")
    x = cl.array.empty(queue, (isize, jsize, ksize),
                       dtype=np.float64, order="F")
    y = cl.array.empty(queue, (isize, jsize, ksize),
                       dtype=np.float64, order="F")
    z = cl.array.empty(queue, (isize, jsize, ksize),
                       dtype=np.float64, order="F")

    cl.clrandom.fill_rand(x)
    cl.clrandom.fill_rand(y)
    a = 1.0
    myprofiler.endtimer("CLInit")

    myprofiler.starttimer("zaxpy3")
    zaxpy3(queue, imin=imin, imax=imax, jmin=jmin,
           jmax=jmax, kmin=kmin, kmax=kmax, a=a, x=x, y=y, z=z)
#    dgemm(queue,roi=roi,a=1.0, x=x, y=y, z=z)
    myprofiler.endtimer("zaxpy3")

    myprofiler.starttimer("CheckResult")
    z_ref = (x.get() + a*y.get())
    errnorm = la.norm(z_ref - z.get())/la.norm(z_ref)
    myprofiler.endtimer("CheckResult")

    assert errnorm < 1e-10


if __name__ == "__main__":

    myprofiler.starttimer()

    datapath = "./"
    numargs = len(sys.argv)
    if numargs > 1:
        datapath = sys.argv[1]
        datapath += "/kernels/"
        print("DataPath: ", datapath)

    myprofiler.starttimer("main")
    main(datapath)
    myprofiler.endtimer("main")
    myprofiler.endtimer()
    myprofiler.writeserialprofile("LoopyZAXPY3_SerialTiming")
