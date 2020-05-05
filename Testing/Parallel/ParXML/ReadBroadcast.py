import sys
import xmltodict
from mpi4py import MPI
import pyjuke as juke
import juketest
import parjuke

comm = MPI.COMM_WORLD
myrank = comm.Get_rank()
numproc =comm.Get_size()
testname = "ReadBroadcast"

def runtest():
    
    path_to_datafile = juke.sourcepath+"/Testing/Data/XML/parxml.xml"

    if myrank == 0:
        filedesc = open(path_to_datafile,"r")
        mydict = xmltodict.parse(filedesc.read())
        print(testname, "mydict = {", mydict, "}")
        filedesc.close()
    else:
        mydict = None

    mydict = comm.bcast(mydict)
    expected_data = 'v1.1.1 v1.1.2 v1.1.3 v1.1.4'
    
    if myrank != 0:
        print("ReadBroadcast(",myrank,"): BC(mydict) = {", mydict, "}")
    
    check_value = 0    
    if mydict['juke_config']['config1']['value1'] !=  expected_data:
        check_value = 1
        print("ReadBroadcast(",myrank,"): Failed test.")
#        print("ReadBroadcast(",myrank,"): Expected: ",expected_data)
#        print("ReadBroadcast(",myrank,"): Got: ",mydict['juke_config']['config1']['value1'])

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
        juketest.updatetestresults(testresult,resultsfilename)
