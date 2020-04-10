# OKLib

This is a stand-alone library of simple computational kernels used in _PlasCom2_. It includes a stand-alone library of kernels 
and self-contained kernelized implementation of the _PlasCom2_ Equation of State (EOS). All kernels, including the EOS, are 
available through a C and Fortran-compatible API. 

# Obtain justkernels
> git clone https:://github.com/illinois-ceesd/justkernels

# Configuration
Configuration requires _CMake_. Out-of-source builds are recommended. 

> mkdir /path/to/build
> cd /path/to/build
> cmake /path/to/justkernels

# Building the kernels

> make [-j]

# Documentation
Building documentation requires Doxygen, and GraphViz is recommended.

> make documentation

After building the documentation, point your browser at:
(/path/to/build/docs/html/index.html). Choose *Computational Kernel Documentation* 
in the left browser menu.






