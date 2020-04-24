import sys
import os
import platform
import stat


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
    resultsFileName = outputPath + "/parallel_" + suiteName + "_results.txt"
    testListFile = suitePath + "/testlist.txt"

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

    outScript = open(scriptFileName, "w")
    if(systemType == "workstation"):
        # Then it's an mpiexec situation
        print("#!/bin/sh\n\n", file=outScript)
        print("rm -f ", resultsFileName, file=outScript)
        print("for testname in $(cat ", testListFile, ")\n", file=outScript)
        print("do\n", file=outScript)
        print("     printf \"", scriptShortName, ": Running test",
              " (${testname})...\\n\"", file=outScript)
        print("     printf \"", scriptShortName, ": Command: ",
              " mpiexec -n 4 python ", suitePath + "/${testname}.py ",
              resultsFileName, "\\n\"\n", file=outScript)
        print("     date\n", file=outScript)
        print("     mpiexec -n 4 python ", suitePath + "/${testname}.py ",
              resultsFileName, "\n", file=outScript)
        print("     date\n", file=outScript)
        print("done\n", file=outScript)

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
