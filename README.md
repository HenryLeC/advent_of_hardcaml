# Advent of FPGA in Hardcaml

This repository includes selected Advent of Code problems implemented in the Hardcaml HDL

## Running Tests

All solutions include expect tests which can be run by running `dune build @runtests` this will crete vcd files in the `out` directory and will verify that the answer computer by the design is correct. You can view a sample of the waveform in the corresponding test file in the `test/day_N` directory.

If you change the test input you can run the rests and then run `dune promote` to update the correct output to the latest test run result.

## Design Documentation

Each design is documentedd in the `docs/` folder. These docs include a description of the algorithm used to come to the solution and some code implementing the described algorithm.