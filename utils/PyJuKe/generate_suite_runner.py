import sys
import os
import platform
import stat
import pyjuke


def generate_suite_runner(suiteName, suitePath, outputPath):

    programName = "generate_suite_runner"

    nodeName = platform.node()
#    systemName = platform.system()
    systemType = "workstation"

    systemId = nodeName.find("quartz")
    if(systemId >= 0):
        systemType = "quartz"

    systemId = nodeName.find("lassen")
    if(systemId >= 0):
        systemType = "lassen"

    scriptShortName = "parallel_"+suiteName
    scriptName = scriptShortName + ".sh"
    scriptFileName = outputPath + "/" + scriptName
    spawnerName = scriptShortName+"_spawner.sh"
    spawnerPath = outputPath+"/"+spawnerName
    resultsFileName = outputPath + "/parallel_" + suiteName + "_results.txt"
    testListFile = suitePath + "/testlist.txt"
    jukePath = pyjuke.jukepath
    jukeSourcePath = pyjuke.sourcepath
    jukeBinPath = pyjuke.binpath

#    testNames = open(testListFile).readlines()
#    numTests = len(testNames)
    #    print(programName,": SuiteName = ",suiteName)
    #    print(programName,": SuitePath = ",suitePath)
    #    print(programName,": OutPath   = ",outputPath)
    #    print(programName,": Machine   = ",platform.machine())
    #    print(programName,": Node      = ",platform.node())
    #    print(programName,": Platform  = ",platform.platform())
    #    print(programName,": Processor = ",platform.processor())
    #    print(programName,": System    = ",platform.system())
    #    print(programName,": UName     = ",platform.uname())

    print(programName, ": Generating script for suite(", suiteName, ")")
    print(programName, ": System Type(", systemType, ")")
    print(programName, ": scriptFileName  = ", scriptFileName)
    print(programName, ": resultsFileName = ", resultsFileName)

    if(systemType == "workstation"):
        print("GenerateSuiteRunner: Generating for user workstation.")
        # Then it's an mpiexec situation
        outScript = open(scriptFileName, "w")
        print("#!/bin/sh\n\n", file=outScript)
        print("numProcs=\"4\"", file=outScript)
        print("if [ ! -z ${1} ]; then numProcs=${1}; fi", file=outScript)
        print("set -x", file=outScript)
        print("export PYOPENCL_CTX='0'", file=outScript)
        print("export PYTHONPATH=\""+jukePath+":${PYTHONPATH}\"",
              file=outScript)
        print("rm -f ", resultsFileName, file=outScript)
        print("for testname in $(cat ", testListFile, ")\n", file=outScript)
        print("do\n", file=outScript)
        print("     printf \"", scriptShortName, ": Running test",
              " (${testname})...\\n\"", file=outScript)
        print("     printf \"", scriptShortName, ": Command: ",
              " mpiexec -n ${numProcs} python ",
              suitePath + "/${testname}.py ",
              resultsFileName, "\\n\"\n", file=outScript)
        print("     date\n", file=outScript)
        print("     mpiexec -n ${numProcs} python ",
              suitePath + "/${testname}.py ",
              resultsFileName, "\n", file=outScript)
        print("     date\n", file=outScript)
        print("done\n", file=outScript)
        outScript.close()
        os.chmod(scriptFileName, stat.S_IRWXU | stat.S_IRWXG | stat.S_IRWXO)

    if(systemType == "quartz"):
        print("GenerateSuiteRunner: Generating for Quartz@LLNL.")
        # We need to generate a suite runner that runs a parallel
        # spawning script. First; the spawning script:
        outScript = open(spawnerPath, "w")
        print("#!/bin/sh\n\n", file=outScript)
        print("numProcs=\"4\"", file=outScript)
        print("if [ ! -z ${1} ]; then numProcs=${1}; fi", file=outScript)
        print("set -x", file=outScript)
        print("export PYOPENCL_CTX=''", file=outScript)
        print("export PYTHONPATH=\""+jukePath+":${PYTHONPATH}\"",
              file=outScript)
        print("rm -f ", resultsFileName, file=outScript)
        print("for testname in $(cat ", testListFile, ")\n", file=outScript)
        print("do\n", file=outScript)
        print("     printf \"", scriptShortName, ": Running test",
              " (${testname})...\\n\"", file=outScript)
        print("     printf \"", scriptShortName, ": Command: ",
              " srun -n ${numProcs} python ",
              suitePath + "/${testname}.py ",
              resultsFileName, "\\n\"\n", file=outScript)
        print("     date\n", file=outScript)
        print("     srun -n ${numProcs} python ",
              suitePath + "/${testname}.py ",
              resultsFileName, "\n", file=outScript)
        print("     date\n", file=outScript)
        print("done\n", file=outScript)
        outScript.close()
        os.chmod(spawnerPath, stat.S_IRWXU | stat.S_IRWXG | stat.S_IRWXO)
        
        # Now the suite runner:
        outScript = open(scriptFileName, "w")
        numNodes = 1
        numProcJob = 36
        numProcTest = 4
        timeOut = 10
        print("#!/bin/bash\n\nset -x\ndate", file=outScript)
        print("salloc -N "+str(numNodes)+" -n "+str(numProcJob)+
              " -t "+str(timeOut)+" -ppdebug "+spawnerPath+" "+
              str(numProcTest), file=outScript)
        print("errorCode=$?", file=outScript)
        print("date", file=outScript)
        print("exit $errorCode", file=outScript)
#        print("if test $errorCode -neq 0\nthen\n  exit(1)\nfi\n", file=outScript)
#        print("exit 0", file=outScript) 
        outScript.close()
        os.chmod(scriptFileName, stat.S_IRWXU | stat.S_IRWXG | stat.S_IRWXO)



if __name__ == "__main__":

    suitePath = "./"
    outputPath = "./"
    suiteName = "ParJuKe"

    numArgs = len(sys.argv)
    if numArgs > 1:
        suiteName = sys.argv[1]
    if numArgs > 2:
        suitePath = sys.argv[2]
    if numArgs > 3:
        outputPath = sys.argv[3]

    generate_suite_runner(suiteName, suitePath, outputPath)
