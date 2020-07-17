from mpi4py import MPI
import numpy as np


def checkerror(errorvalue=0, comm=MPI.COMM_WORLD, rootrank=0):

    my_error = np.zeros(1)
    par_error = np.zeros(1)

    my_error[0] = errorvalue

    comm.Reduce(my_error, par_error, MPI.MAX, rootrank)

    return par_error[0]


def xmlreadbc(pathtofile, comm=MPI.COMM_WORLD):
    import xmltodict

    myrank = comm.Get_rank()

    if myrank == 0:
        fd = open(pathtofile, "r")
        mydict = xmltodict.parse(fd.read())
        fd.close()
    else:
        mydict = None

    mydict = comm.bcast(mydict)

    return(mydict)
