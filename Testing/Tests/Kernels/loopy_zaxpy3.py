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
import pyjuke

mpiCommObj = MPI.COMM_WORLD
myProfiler = Profiler(mpiCommObj)


def main(dataPath):

    global myProfiler

    fn = dataPath+"zaxpy3.f90"

    with open(fn, "r") as inf:
        source = inf.read()

    myProfiler.StartTimer("LoopyParse")
#    dgemm, = lp.parse_transformed_fortran(source, filename=fn)
    zaxpy3, = lp.parse_fortran(source, filename=fn)
    zaxpy3 = lp.set_options(zaxpy3, write_code=True)
    myProfiler.EndTimer("LoopyParse")

    myProfiler.StartTimer("CLContext")
#    ctx = cl.create_some_context(False, pyjuke.cl_context_answers)
    ctx = cl.create_some_context(False)
    queue = cl.CommandQueue(ctx)
    myProfiler.EndTimer("CLContext")

    iSize = 128
    jSize = 128
    kSize = 128

    imin = 1
    jmin = 1
    kmin = 1

    imax = iSize
    jmax = jSize
    kmax = kSize

    myProfiler.StartTimer("CLInit")
    x = cl.array.empty(queue, (iSize, jSize, kSize),
                       dtype=np.float64, order="F")
    y = cl.array.empty(queue, (iSize, jSize, kSize),
                       dtype=np.float64, order="F")
    z = cl.array.empty(queue, (iSize, jSize, kSize),
                       dtype=np.float64, order="F")

    cl.clrandom.fill_rand(x)
    cl.clrandom.fill_rand(y)
    a = 1.0
    myProfiler.EndTimer("CLInit")

    myProfiler.StartTimer("zaxpy3")
    zaxpy3(queue, imin=imin, imax=imax, jmin=jmin,
           jmax=jmax, kmin=kmin, kmax=kmax, a=a, x=x, y=y, z=z)
#    dgemm(queue,roi=roi,a=1.0, x=x, y=y, z=z)
    myProfiler.EndTimer("zaxpy3")

    myProfiler.StartTimer("CheckResult")
    z_ref = (x.get() + a*y.get())
    errNorm = la.norm(z_ref - z.get())/la.norm(z_ref)
    myProfiler.EndTimer("CheckResult")

    assert errNorm < 1e-10


if __name__ == "__main__":

    myProfiler.StartTimer()

    dataPath = "./"
    numArgs = len(sys.argv)
    if numArgs > 1:
        dataPath = sys.argv[1]
        dataPath += "/kernels/"
        print("DataPath: ", dataPath)

    myProfiler.StartTimer("main")
    main(dataPath)
    myProfiler.EndTimer("main")
    myProfiler.EndTimer()
    myProfiler.WriteSerialProfile("LoopyZAXPY3_SerialTiming")
