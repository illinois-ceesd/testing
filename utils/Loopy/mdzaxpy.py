import numpy as np
import numpy.linalg as la
import pyopencl as cl
import pyopencl.array
import pyopencl.clrandom
import loopy as lp


def main():
    fn = "zaxpy3.f90"
    with open(fn, "r") as inf:
        source = inf.read()

#    dgemm, = lp.parse_transformed_fortran(source, filename=fn)
    zaxpy3, = lp.parse_fortran(source, filename=fn)
    zaxpy3 = lp.set_options(zaxpy3, write_code=True)
    1/0

#    ctx = cl.create_some_context()
#    queue = cl.CommandQueue(ctx)

#    n   = 128
#    x   = cl.array.empty(queue, (n, n, n), dtype=np.float64, order="F")
#    y   = cl.array.empty(queue, (n, n, n), dtype=np.float64, order="F")
#    z   = cl.array.empty(queue, (n, n, n), dtype=np.float64, order="F")
#    roi = cl.array.empty(queue,(6),dtype=np.float64,order="F")

#    roi[0] = 1
#    roi[1] = 128
#    roi[2] = 1
#    roi[3] = 128
#    roi[4] = 1
#    roi[5] = 128

#    cl.clrandom.fill_rand(a)
#    cl.clrandom.fill_rand(b)


#    dgemm(queue,roi=roi,a=1, x=x, y=y, z=z)

#    z_ref = (x.get() + y.get())
#    assert la.norm(z_ref - z.get())/la.norm(z_ref) < 1e-10


if __name__ == "__main__":
    main()
