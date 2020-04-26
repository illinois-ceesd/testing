# Testing 

In the following documentation, these platform-and-installation-specific file system paths will be used:

- PROJECTSRC: The path to your clone of the project repo
- PROJECTBIN: The path to your build directory

The current testing infrastructure is capable of detecting and driving
tests and suites of serial and parallel tests on most platforms.
To exercise the project's tests, after *cmake* and *make*, use:

> make test  

-or-

> make Experimental (submits results to the [Dashboard](https://my.cdash.org/index.php?project=JustKernels))

The output stdout/stderr created by the testing infrastructure and
the tests themselves is collected in:

> ${PROJECTBIN}/Testing/Temporary/LastTest.log

Automated test detection and driving mechanisms are implemented
for the following types of tests:

 1. Suites of serial tests in Python
 1. Suites of parallel tests in Python

The following sections will outline the automated system
for detecting and driving tests, and cover how to add new
tests and testing suites to the system.

# Serial Python tests

The infrastructure for serial python tests is implemented in *CMake*
in the file (${PROJECTSRC}/Testing/CMakeLists.txt). At configuration-time,
the testing infrastructure looks at the file:

> ${PROJECTSRC}/Testing/testdirectory.txt

*CMake* will automatically check the file (testdirectory.txt) for
a list of suites.  Each line of testdirectory.txt should contain a
TESTPATH.

For each TESTPATH, *CMake* will look for the file named
(${PROJECTSRC}/Testing/${TESTPATH}/testlist.txt). Each line of testlist.txt
should contain a TESTNAME.

For each TESTNAME, CMake will add a test named ${TESTNAME} that
executes the following command:

> python ${PROJECTSRC}/Testing/${TESTPATH}/${TESTNAME}.py ${PROJECTSRC} ${PROJECTBIN}

The return code of the above command indicates whether the 
test passes or fails. (0=pass)

*Note: If the TESTNAME contains the word "FAILS", then
CMake will expect the test to FAIL and automatically 
flips the meaning of the exit code (0=fail).

## Adding tests to an existing suits

To ADD a test into an existing directory, add your test in
${PROJECTSRC}/Testing/${TESTPATH}/${TESTNAME}.py, and add a line for your test
in ${PROJECTSRC}/Testing/${TESTPATH}/testlist.txt.

** *CMake* must be re-run after adding new tests.**

## Adding new serial suites

To ADD a new testing directory, make the directory and add
it to ./testdirectory.txt.

** *CMake* must be re-run after adding new test suites.**

## Example serial test(s)

Look at any Python files in the Testing (i.e. *this*)
directory tree for testing examples. There is a set of
empty testing templates provided in:

> ./Examples/Example*.py

# Parallel Python tests

Parallel testing is a little more troublesome than serial 
testing.  Parallel tests often need to navigate a system's 
resource management stack to get at the compute resources 
needed to run the tests.

The infrastructure for parallel python tests is implemented 
in *CMake* in the file (${PROJECTSRC}/Testing/CMakeLists.txt).  
In short, parallel tests are arranged more strictly into 
*parallel suites*.  Each suite executes in its own *batch* 
through a suite-and-platform-specific *suite runner* script.

Each of the components and procedures for the parallel testing 
is described in the following sections.  Adding new parallel tests 
and suites is also covered.

## Configuration-time infrastructure

At configuration-time (i.e. when the user issues *cmake*), the testing 
infrastructure looks at the file:

> ${PROJECTSRC}/Testing/Parallel/parallelsuites.txt

*CMake* will automatically check the file (parallelsuites.txt) for
a list of parallel suites.  Each line of parallelsuites.txt
should contain a SUITENAME.

For each SUITENAME, *CMake* will add 2 tests:

 - Parallel:${SUITENAME}:Setup (creates a batch script for the suite)
 - Parallel:${SUITENAME}:Run   (runs the batch script for the suite)

For each SUITENAME, *CMake* will also look for the file named
(${PROJECTSRC}/Testing/Parallel/${SUITENAME}/testlist.txt). Each line of
testlist.txt should contain a TESTNAME.

For each TESTNAME, *CMake* will add the following test:

 - Parallel:${SUITENAME}:${TESTNAME}
 
For each TESTNAME, *CMake* will also add a line to the suite runner or batch
script executing the equivalent of the following:

> mpiexec -n <numProc> python ${PROJECTSRC}/Testing/Parallel/${SUITENAME}/${TESTNAME}.py ${PROJECTBIN}/Testing/parallel_${SUITENAME}_results.txt

A single argument is passed to each parallel test which contains the
path to the output file where all tests from the parallel suite will
store their results.

## Test-time infrastructure

At testing-time (i.e. when the user issues *make test*), the *Setup* part
of the parallel suite runs in the test called (Parallel:${SUITENAME}:Setup)
and executs the following command:

> ${PROJECTSRC}/utils/PyJuKe/generate_suite_runner.py ${SUITENAME} ${PROJECTSRC}/Testing/Parallel/${SUITENAME} ${PROJECTBIN}/Testing

The Setup test creates the platform-specific script (called the *suite runner*) 
that will execute the suite of parallel tests in its own batch script if required. The
platform-specific suite runner will be written to:

> ${PROJECTBIN}/Testing/parallel_${SUITENAME}.sh

Following the creation of the suite runner script, *CMake* will execute
the test named (Parallel:${SUITENAME}:Run), which executes the parallel
suite runner script which will be responsible for navigating the batch
queue and running all the tests in the suite.

The PASS/FAIL results of all tests in a parallel suite are collected to the file:

> ${PROJECTBIN}/Testing/parallel_${SUITENAME}_results.txt

After the suite runner completes, all tests inside the suite have been
executed and their results stored to the results file.  For each of the
tests named in (${PROJECTSRC}/Parallel/${SUITENAME}/testlist.txt), The
infrastructure then runs the test named (Parallel:${SUITENAME}:${TESTNAME})
which simply checks the results with the following command:

> python ${PROJECTSRC}/utils/Checker/CheckTest.py ${TESTNAME} ${PROJECTBIN}/Testing/parallel_${SUITENAME}_results.txt

This command returns a 0 (pass), or a 1 (fail).  A message about the
cause of failure is also written to the stdout/stderr for the testing
and can be found in ${PROJECTBIN}/Testing/Temporary/LastTest.log.

# Testing configuration options

The following *CMake* options are available for configuration
of the testing infrastructure.

Disable tests altogether:
> -DBUILD_TESTS=ON|OFF [ON]

Show test names at configuration time:
> -DSHOWESTS=ON|OFF [OFF]


