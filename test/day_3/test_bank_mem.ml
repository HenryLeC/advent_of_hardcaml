open! Core
open! Hardcaml
open! Bits
module Waveform = Hardcaml_waveterm.Waveform
module Bank_mem = Hardcaml_day3.Bank_mem
module Sim = Cyclesim.With_interface (Bank_mem.I) (Bank_mem.O)

let testbench (test : Sim.t -> t ref Bank_mem.I.t -> t ref Bank_mem.O.t -> unit) =
  let scope = Scope.create ~flatten_design:true () in
  let sim = Sim.create (Bank_mem.hierarchical scope "test_instance") in
  let waves, sim = Waveform.create sim in
  let inputs = Cyclesim.inputs sim in
  let outputs = Cyclesim.outputs sim in
  test sim inputs outputs;
  Cyclesim.cycle sim;
  Waveform.print waves
;;

let%expect_test "write_one_test" =
  testbench (fun sim inputs outputs ->
    inputs.write_lsp := Bits.of_int ~width:4 5;
    inputs.read_idx := Bits.of_int ~width:4 1;
    inputs.write_mask := Bits.of_int ~width:12 3;
    Printf.printf
      "Output: %d Output_prev: %d\n"
      (Bits.to_int !(outputs.read_val))
      (Bits.to_int !(outputs.read_prev_val));
    Cyclesim.cycle sim;
    Printf.printf
      "Output: %d Output_prev: %d\n"
      (Bits.to_int !(outputs.read_val))
      (Bits.to_int !(outputs.read_prev_val));
    Cyclesim.cycle sim;
    Printf.printf
      "Output: %d Output_prev: %d\n"
      (Bits.to_int !(outputs.read_val))
      (Bits.to_int !(outputs.read_prev_val)));
  [%expect
    {|
    Output: 0 Output_prev: 0
    Output: 0 Output_prev: 5
    Output: 5 Output_prev: 5
    ┌Signals────────┐┌Waves──────────────────────────────────────────────┐
    │clock          ││┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌──│
    │               ││    └───┘   └───┘   └───┘   └───┘   └───┘   └───┘  │
    │               ││────────────────────────                           │
    │read_idx       ││ 1                                                 │
    │               ││────────────────────────                           │
    │               ││────────────────────────                           │
    │write_lsp      ││ 5                                                 │
    │               ││────────────────────────                           │
    │               ││────────────────────────                           │
    │write_mask     ││ 003                                               │
    │               ││────────────────────────                           │
    │               ││────────┬───────────────                           │
    │read_prev_val  ││ 0      │5                                         │
    │               ││────────┴───────────────                           │
    │               ││────────────────┬───────                           │
    │read_val       ││ 0              │5                                 │
    │               ││────────────────┴───────                           │
    │               ││                                                   │
    └───────────────┘└───────────────────────────────────────────────────┘
    |}]
;;
