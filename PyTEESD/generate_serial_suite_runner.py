import sys
from teesd import generate_suite_runner


if __name__ == "__main__":

    suitepath = "./"
    outputpath = "./"
    suitename = "SerialTEESD"

    numargs = len(sys.argv)
    if numargs > 1:
        suitename = sys.argv[1]
    if numargs > 2:
        suitepath = sys.argv[2]
    if numargs > 3:
        outputpath = sys.argv[3]

    generate_suite_runner(suitename, suitepath, outputpath,
                          nnodes=1, nproc=1)
