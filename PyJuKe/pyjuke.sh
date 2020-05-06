#!/bin/bash

SRCPATH=${1}

if [ ! -z ${2} ]; then
    BINPATH=${2}
    OUTDIR=${BINPATH}/PyJuKe
    OUTFILE=${OUTDIR}/pyjuke.py
fi

if [ ! -z ${3} ]; then GETREPO=${3}; fi

ORIGPATH=`pwd`
BUILDHOST=`hostname -f`
BUILDDATE=`date`

cd ${SRCPATH}

if [ "${GETREPO}" != "" ]; then
    
    REPONAME=`git ls-remote 2>&1 | grep From | grep git | cut -d " " -f 2`
    BRANCHNAME=`git branch | grep "*" | cut -d " " -f 2-`
    LOCALREV=`git rev-parse HEAD`
    BRANCHREFNAME=`git symbolic-ref -q HEAD`
    if [ "${BRANCHREFNAME}" != "" ]; then
        UPSTREAM=`git for-each-ref --format='%(upstream:short)' ${BRANCHREFNAME}`
        REMOTEREV=`git rev-parse ${UPSTREAM}`
    else
        # Detached HEAD
        REMOTEREV=""
    fi
    CHANGED=`git diff --name-only | tr '\n' ' '`
    REVISION=""
    if [ "$LOCALREV" == "$REMOTEREV" ]; then
        REVISION=${LOCALREV}
    fi
fi

if [ ! -e ${OUTDIR} ]; then mkdir -p ${OUTDIR}; fi

if [ "$OUTFILE" != "" ]; then
    
    
    if [ -e ${OUTFILE} ]; then rm ${OUTFILE}; fi
    
    printf "sourcepath = \"${SRCPATH}\"\n"       > ${OUTFILE}
    printf "binpath = \"${BINPATH}\"\n"         >> ${OUTFILE}
    printf "reponame = \"${REPONAME}\"\n"       >> ${OUTFILE}
    printf "branchname = \"${BRANCHNAME}\"\n"   >> ${OUTFILE}
    printf "revision = \"${REVISION}\"\n"       >> ${OUTFILE}
    printf "localrev = \"${LOCALREV}\"\n"       >> ${OUTFILE}
    printf "remoterev = \"${REMOTEREV}\"\n"     >> ${OUTFILE}
    printf "buildhost = \"${BUILDHOST}\"\n"     >> ${OUTFILE}
    printf "builddate = \"${BUILDDATE}\"\n"     >> ${OUTFILE}
    printf "changed = \"${CHANGED}\"\n"         >> ${OUTFILE}
    printf "jukepath = \"${BINPATH}/PyJuKe\"\n" >> ${OUTFILE}
    printf "cl_context_answers = [0, 0]\n"      >> ${OUTFILE}
    
else
    
    printf "sourcepath = \"${SRCPATH}\"\n"
    printf "binpath    = \"${BINPATH}\"\n"   
    printf "reponame   = \"${REPONAME}\"\n"      
    printf "branchname = \"${BRANCHNAME}\"\n" 
    printf "revision   = \"${REVISION}\"\n"     
    printf "localrev   = \"${LOCALREV}\"\n"     
    printf "remoterev  = \"${REMOTEREV}\"\n"   
    printf "buildhost  = \"${BUILDHOST}\"\n"   
    printf "builddate  = \"${BUILDDATE}\"\n"   
    printf "changed    = \"${CHANGED}\"\n"       
    printf "cl_context_answers = [0, 0]\n"    
    printf "jukepath = \"${BINPATH}/PyJuKe\"\n"
    
fi

# =========== copy ALL the Python to the build/bin place =====
cp ${SRCPATH}/PyJuKe/*.py ${OUTDIR}

cd ${ORIGPATH}
