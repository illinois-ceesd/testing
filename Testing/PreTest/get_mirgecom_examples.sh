#!/bin/sh

sourcedir=${1}
bindir=${2}

printf "get_mirgecom_examples: SourcePath = ${sourcedir}\n"
printf "get_mirgecom_examples: BinPath    = ${bindir}\n"

cp ${bindir}/mirgecom/examples/lump.py .
cp ${bindir}/mirgecom/examples/vortex.py .
cp ${bindir}/mirgecom/examples/lump-mpi.py .
cp ${bindir}/mirgecom/examples/vortex-mpi.py .
cp ${bindir}/mirgecom/examples/wave-eager.py .
