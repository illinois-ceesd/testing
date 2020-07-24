#!/bin/sh

sourcedir=${1}
bindir=${2}

printf "get_mirgecom_examples: SourcePath = ${sourcedir}\n"
printf "get_mirgecom_examples: BinPath    = ${bindir}\n"

cp ${sourcedir}/emirge/mirgecom/examples/lump2.py .
cp ${sourcedir}/emirge/mirgecom/examples/vortex.py .
