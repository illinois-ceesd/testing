# OKLib

This is a stand-alone library of simple computational kernels used in PlasCom2. It includes a stand-alone library of kernels 
and self-contained kernelized implementation of the PlasCom2 Equation of State (EOS). 

# Obtain justkernels
> git clone https:://github.com/illinois-ceesd/justkernels

# Configuration
Configuration requires _CMake_. Out-of-source builds are recommended. 

> mkdir /path/to/build\n
> cd /path/to/build\n
> cmake /path/to/justkernels\n

# Building the kernels
> make [-j]

# Documentation
Building documentation requires Doxygen, and GraphViz is recommended.
> make documentation
After building the documentation, point your browser at:
(/path/to/build/docs/html/index.html). Choose *Computational Kernel Documentation* 
in the left browser menu.






