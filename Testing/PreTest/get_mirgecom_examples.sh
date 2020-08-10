#!/bin/sh

sourcedir=${1}
bindir=${2}

printf "get_mirgecom_examples: SourcePath = ${sourcedir}\n"
printf "get_mirgecom_examples: BinPath    = ${bindir}\n"

cp ${sourcedir}/mirgecom/examples/lump.py .
cp ${sourcedir}/mirgecom/examples/vortex.py .
cp ${sourcedir}/mirgecom/examples/lump-mpi.py .
cp ${sourcedir}/mirgecom/examples/vortex-mpi.py .
cp ${sourcedir}/mirgecom/examples/wave-eager.py .
