import sys
from mpi4py import MPI
import pyjuke as juke
import ABaTe as abate
import parjuke

comm = MPI.COMM_WORLD
myrank = comm.Get_rank()
numproc = comm.Get_size()
testname = "JuKeReadBC"

def runtest():
    
    path_to_datafile = juke.sourcepath+"/Testing/Data/XML/parxml.xml"

    mydict = parjuke.xmlreadbc(path_to_datafile, comm)
    
    expected_data = 'v1.1.1 v1.1.2 v1.1.3 v1.1.4'
    
    check_value = 0    
    if mydict['juke_config']['config1']['value1'] !=  expected_data:
        check_value = 1
        print(testname,"(",myrank,"): Failed test.")
        print(testname,"(",myrank,"): Expected: ",expected_data)
        print(testname,"(",myrank,"): Got: ",mydict['juke_config']['config1']['value1'])

    check_value = parjuke.checkerror(check_value)

    return(check_value)


if __name__ == "__main__":

    import sys

    numargs = len(sys.argv)
    resultsfilename = ""
    if numargs > 1:
        resultsfilename = sys.argv[1]

    myrank = comm.Get_rank()

    test_pass = runtest()
    testresult = {testname: "Pass"}

    if test_pass != 0:
        testresult = {testname: "Fail"}
        
    if myrank == 0:
        abate.updatetestresults(testresult,resultsfilename)
