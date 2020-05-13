#!/usr/bin/env python
# -*- coding: utf-8 -*-
from __future__ import absolute_import, print_function
import numpy as np
import pyopencl as cl
import time
import sys


from mpi4py import MPI
import loopy as lp
from profiler import Profiler
import pyjuke as jk
import ABaTe as abate

mpicommobj = MPI.COMM_WORLD
myprofiler = Profiler(mpicommobj)
testname = "zaxpy_pyopencl"
maxsize = pow(1024,3) / 10


def main():

    global myprofiler

    size = 1024

    while size < maxsize:
        
        a_np = np.random.rand(size).astype(np.float64)
        b_np = np.random.rand(size).astype(np.float64)

        # a_np = np.full(size,1.5).astype(np.float64)
        # b_np = np.full(size,2.3).astype(np.float64)

        ctx = cl.create_some_context()
        queue = cl.CommandQueue(ctx)

        mf = cl.mem_flags
        a_g = cl.Buffer(ctx, mf.READ_ONLY | mf.COPY_HOST_PTR, hostbuf=a_np)
        b_g = cl.Buffer(ctx, mf.READ_ONLY | mf.COPY_HOST_PTR, hostbuf=b_np)
        
        startname = 'start-'+str(size)
        myprofiler.starttimer(startname)
        
        cl.enqueue_copy(queue, a_g, a_np)
        cl.enqueue_copy(queue, b_g, b_np)



        prg = cl.Program(ctx, """
        __kernel void sum(
        __global const double *a_g, __global const double *b_g, __global double *res_g)
        {
        int gid = get_global_id(0);
        res_g[gid] = 23.0 * a_g[gid] + b_g[gid];
        }
        """).build()

        res_g = cl.Buffer(ctx, mf.WRITE_ONLY, a_np.nbytes)
        cl.enqueue_copy(queue, b_g, b_np)
        cl.enqueue_barrier(queue)

        # Warmup
        prg.sum(queue, a_np.shape, None, a_g, b_g, res_g)
        queue.finish()

        queuename = 'zaxpy-' + str(size)
        myprofiler.starttimer(queuename)
        for i in range(10):
            prg.sum(queue, a_np.shape, None, a_g, b_g, res_g)
            queue.finish()

        myprofiler.endtimer(queuename)
    myprofiler.endtimer(startname)
    size *= 2
    
# _get_time(queue, prg.sum)


# res_np = np.empty_like(a_np)
# cl.enqueue_copy(queue, res_np, res_g)

# print(res_np[12])

# Check on CPU with Numpy:
# print(res_np - (a_np + b_np))
# print(np.linalg.norm(res_np - (a_np + b_np)))
# assert np.allclose(res_np, a_np + b_np)

if __name__ == "__main__":

    myprofiler.starttimer()

    myprofiler.starttimer("runtest")
    main()
    myprofiler.endtimer("runtest")

    numargs = len(sys.argv)
    outfilename = ""

    if numargs > 1:
        outfilename = sys.argv[1]

    testresult = {testname: "Pass"}
    abate.updatetestresults(testresult, outfilename)

    myprofiler.endtimer()
    myprofiler.writeserialprofile("zaxpy_pyopencl_timing")
