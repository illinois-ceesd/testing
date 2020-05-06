import sys
import ABaTe as abate

numargs = len(sys.argv)
if(numargs != 3):
    sys.exit(1)

testname = sys.argv[1]
resultsfilename = sys.argv[2]

print("CheckTest: Checking ", resultsfilename,
      " for Test(", testname, ")")

testresults = abate.readtestresults(resultsfilename)
checktest = abate.checkresult(testresults, testname)

if(checktest == 0):
    print("CheckTest: Test("+testname+") PASSED.")
    sys.exit(0)
else:
    if(checktest > 0):
        print("CheckTest: Test("+testname+") FAILED.")
    else:
        print("CheckTest: Test("+testname+") MISSING.")
    sys.exit(1)
sys.exit(1)