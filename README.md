# Lassen nightly testing results:

[![isolator-nightly-lassen](https://github.com/illinois-ceesd/testing/actions/workflows/isolator-nightly-lassen.yaml/badge.svg)](https://github.com/illinois-ceesd/testing/actions/workflows/isolator-nightly-lassen.yaml)
[![CI](https://github.com/illinois-ceesd/testing/actions/workflows/ci.yaml/badge.svg)](https://github.com/illinois-ceesd/testing/actions/workflows/ci.yaml)

# Testing - nightly testing for *MIRGE-Com*

This package is a general testing suite encapsulation useful for automating tests on remote platforms with resource management systems.  The package is demonstrated by implementing nightly testing for *MIRGE-Com* on Lassen.  More tests and platforms should be easy for the user to add.  Test results should be reported in CI.

## The following tests are currently in the nightly testing suite on Lassen for *MIRGE-Com*:

### *MIRGE-Com* testing suite: `mirgecom`

The *MIRGE-Com* testing suite is defined in `mirgecom/testing` and implements the Lassen nightly testing in the `mirgecom/testing/test-lassen.sh` script. Individual testing suites of *MIRGE-Com* include the tests in the following sections.

#### *MIRGE-Com* examples: `mirgecom_examples`

Tests and examples are exercised identically to how they are run in *MIRGE-Com*-native CI.  Serial and distrbuted lazy tests and examples use multiple GPUs in the batch queue on Lassen and report their output and test results to this testing CI.

### Isolator testing suite: `isolator`

The production driver `isolator` defines its own suite of tests in its `isolator/testing` directory and implements the Lassen nightly testing in the `isolator/testing/test-lassen.sh` script.  The `isolator` testing suite includes the following tests:

#### 3D injection with reactive mixture: `serial-3d-combustion`

This test uses the `smoke_test_injection_3d` configuration and eigthX grid to exercise a serial (single GPU) test of the current version of a 3d case with full combustion (i.e., nspecies=7).  The case is run in lazy, and will eventually include tests for the timing, and parallel (multi-GPU) runs.

#### More tests to come
