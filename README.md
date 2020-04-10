# JustKernels

This is a stand-alone library of simple computational kernels used in _PlasCom2_. It includes a stand-alone library of kernels 
and self-contained kernelized implementation of the _PlasCom2_ Equation of State (EOS). All kernels, including the EOS, are 
available through a C and Fortran-compatible API. 

# Clone 
> git clone https:://github.com/illinois-ceesd/justkernels

# Configure
Configuration requires [_CMake_](https://cmake.org). Out-of-source builds are recommended. 

> mkdir /path/to/build\
> cd /path/to/build\
> cmake /path/to/justkernels

# Build the kernels
Fortran and C++ compilers are required to build the kernels.
> make [-j]

# Build the documentation
Building documentation requires [Doxygen](https://doxygen.nl), and [GraphViz](https://graphviz.org) is recommended.

> make documentation

After building the documentation, point your web browser at:
`/path/to/build/docs/html/index.html`. Choose *Computational Kernel Documentation* 
in the left browser menu.






