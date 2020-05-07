import sys
# the pyjuke module loads some JuKe environment stuff
# It comes from /justkernels/build/directory/PyJuke
# PyJuke/pyjuke.py is created at justkernesl build-time
# import pyjuke


def main(datapath):

    print("Example1: Success! Path(",
          datapath, ")")


if __name__ == "__main__":

    print("TestName: Example1")
    datapath = "./"
    # Incoming sys.path should already include
    # /justkernels/build/path/PyJuke which was
    # added by justkernels/source/path/Testing/CMakeLists.txt
    # from whence this example is called.
    print("Example1: System Paths: ", sys.path)

    # Arguments to all Python tests are:
    # (1) The full path to the JustKernels source code
    # (2) The full path to the JustKernels top-level build
    print("Example1: Incoming Paths: ", sys.argv[1:])

    # For example, data (and other stuff) can be found
    # relative to the incoming paths.
    numargs = len(sys.argv)
    if numargs > 1:
        datapath = sys.argv[1]
        datapath += "/kernels/"
        print("Example1: DataPath: ", datapath)

    main(datapath)
