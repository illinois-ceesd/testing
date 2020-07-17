import sys
import os
import subprocess
import teesd
import abate

testname = "RunStream"
arraysize = 1024
maxarraysize = pow(1024,3) / 10
mypwd = os.getcwd()
srcpath = teesd.sourcepath
testpath = srcpath + '/Testing/Serial/Stream'
streampath = testpath + '/stream.c'
makestream = testpath + '/makestream.sh'
runstream = testpath + '/runstream.sh'
outroot = mypwd + '/stream_output_'
streamroot = mypwd + '/stream_'
outdatapath = mypwd + '/stream_data.txt'

numargs = len(sys.argv)

outfilename = ""

if numargs > 1:
    outfilename = sys.argv[1]

if os.path.exists(outdatapath):
    os.remove(outdatapath)
datafile = open(outdatapath,"w")

while arraysize < maxarraysize:
    print(testname +
          ': Running STREAM with STREAM_ARRAY_SIZE=' +
          str(arraysize))

    mystream = streamroot + str(arraysize)
    print(testname +
          ': Command: ' + makestream + ' ' + streampath +
          ' ' + str(arraysize) + ' ' + mystream)
    sys.stdout.flush()
    
    mycommand = [makestream, streampath, str(arraysize), mystream]
    proc = subprocess.run(mycommand)
    if proc.returncode != 0:
        print(testname +
              ':Error: Failed to build stream executable.')
        sys.exit(1)
        
    outfile = outroot + str(arraysize) + '.txt'
    
    mycommand = [runstream, mystream, outfile]
    proc = subprocess.run(mycommand)
    if proc.returncode != 0:
        print(testname +
              ':Error: Failed to run stream(' + mystream + ').')
        sys.exit(1)
        
    os.remove(mystream)

    mycommand = ["grep","Triad",outfile]
    proc = subprocess.run(mycommand, capture_output=True)
    if proc.returncode != 0:
        print(testname +
              ':Error: Failed to process stream output.')
        sys.exit(1)

    os.remove(outfile)
    
    bandwidth = proc.stdout
    bwstr = bandwidth.decode("utf-8")
    bwtok = bwstr.split()
    dsize = 24 * arraysize
    print(str(dsize)+'    '+bwtok[1], file=datafile)
    arraysize *= 2

datafile.close()

testresult = {testname: "Pass"}
abate.updatetestresults(testresult, outfilename)
# print(testresult)
# print(outfilename)
