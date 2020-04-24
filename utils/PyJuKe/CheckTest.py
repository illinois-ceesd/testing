import re
import sys

numArgs = len(sys.argv)
if(numArgs != 3):
    sys.exit(1)

testName = sys.argv[1]
resultsFileName = sys.argv[2]

print("CheckTest: Checking ", resultsFileName,
      " for Test(", testName, ")")

resultsFile = open(resultsFileName, 'r')

for line in resultsFile:
    if(re.search(testName, line)):
        if(re.search("PASS", line)):
            print("CheckTest: Test(", testName, ") PASSED.")
            sys.exit(0)
        else:
            print("CheckTest: Test(", testName, ") FAILED.")
            sys.exit(1)
print("CheckTest: Test(", testName, ") MISSING.")
sys.exit(1)
