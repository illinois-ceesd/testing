import sys
import juketest

numArgs = len(sys.argv)
if(numArgs != 3):
    sys.exit(1)

testName = sys.argv[1]
resultsFileName = sys.argv[2]

print("CheckTest: Checking ", resultsFileName,
      " for Test(", testName, ")")

testResults = juketest.ReadTestResults(resultsFileName)
checkTest = juketest.CheckResult(testResults, testName)

if(checkTest == 0):
    print("CheckTest: Test("+testName+") PASSED.")
    sys.exit(0)
else:
    if(checkTest > 0):
        print("CheckTest: Test("+testName+") FAILED.")
    else:
        print("CheckTest: Test("+testName+") MISSING.")
    sys.exit(1)
sys.exit(1)
