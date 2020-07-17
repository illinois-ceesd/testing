import sys
# the teesd module loads some TEESD environment stuff
# It comes from /teesd/build/directory/PyTEESD
# PyTEESD/teesd.py is created at TEESD build-time
# import teesd


def main(datapath):

    print("Example1: Success! Path(",
          datapath, ")")


if __name__ == "__main__":

    print("TestName: Example1")
    datapath = "./"
    # Incoming sys.path should already include
    # /teesd/build/path/PyTEESD which was
    # added by teesd/source/path/Testing/CMakeLists.txt
    # from whence this example is called.
    print("Example1: System Paths: ", sys.path)

    # Arguments to all Python tests are:
    # (1) The full path to the TEESD source code
    # (2) The full path to the TEESD top-level build
    print("Example1: Incoming Paths: ", sys.argv[1:])

    # For example, data (and other stuff) can be found
    # relative to the incoming paths.
    numargs = len(sys.argv)
    if numargs > 1:
        datapath = sys.argv[1]
        datapath += "/kernels/"
        print("Example1: DataPath: ", datapath)

    main(datapath)
