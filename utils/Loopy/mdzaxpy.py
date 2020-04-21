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


    ctx = cl.create_some_context()
    queue = cl.CommandQueue(ctx)
 
    m   = 128
    n   = 128
    l   = 128
    imin = 1
    jmin = 1
    kmin = 1
    imax = m
    jmax = n
    kmax = l

    x   = cl.array.empty(queue, (m, n, l), dtype=np.float64, order="F")
    y   = cl.array.empty(queue, (m, n, l), dtype=np.float64, order="F")
    z   = cl.array.empty(queue, (m, n, l), dtype=np.float64, order="F")

    cl.clrandom.fill_rand(x)
    cl.clrandom.fill_rand(y)
    a = 1.0

    zaxpy3(queue,imin=imin,imax=imax,jmin=jmin,jmax=jmax,kmin=kmin,kmax=kmax,a=a,x=x,y=y,z=z)
#    dgemm(queue,roi=roi,a=1.0, x=x, y=y, z=z)

    z_ref = (x.get() + a*y.get())
    assert la.norm(z_ref - z.get())/la.norm(z_ref) < 1e-10


if __name__ == "__main__":
    main()
