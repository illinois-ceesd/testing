def ReadTestResults(fileName):

    from os import path

    testResults = {}

    if fileName != "":
        if path.exists(fileName):
            myFile = open(fileName, "r")
            for line in myFile:
                testName, testResult = line.split()
                testResults[testName] = testResult
            myFile.close()

    return testResults


def WriteTestResults(testResults, fileName=""):

    import sys as mysys

    resultFile = mysys.stdout

    if fileName != "":
        resultFile = open(fileName, "w")

    for testName in testResults:
        print(testName, testResults[testName], file=resultFile)

    if fileName != "":
        resultFile.close()


def UpdateTestResults(testResults, fileName=""):

    allResults = ReadTestResults(fileName)

    for testName in testResults:
        allResults[testName] = testResults[testName]

    WriteTestResults(allResults, fileName)


def CheckResult(testResults, testName):

    if testName in testResults:
        testResult = testResults[testName]
        if (
            testResult.lower() == "pass"
            or testResult.lower() == "passed"
            or testResult.lower() == "yes"
            or testResult.lower() == "true"
            or testResult.lower() == ".true."
            or testResult.lower() == "1"
        ):
            return 0
        else:
            return 1
    return -1
