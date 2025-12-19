open! Core
open! Hardcaml
open! Bits
module Waveform = Hardcaml_waveterm.Waveform
module Decrement_fifo = Hardcaml_day3.Decrement_fifo
module Sim = Cyclesim.With_interface (Decrement_fifo.I) (Decrement_fifo.O)

let testbench
      (test : Sim.t -> t ref Decrement_fifo.I.t -> t ref Decrement_fifo.O.t -> unit)
  =
  let scope = Scope.create ~flatten_design:true () in
  let sim = Sim.create (Decrement_fifo.hierarchical scope "test_instance") in
  let waves, sim = Waveform.create sim in
  let inputs = Cyclesim.inputs sim in
  let outputs = Cyclesim.outputs sim in
  test sim inputs outputs;
  Cyclesim.cycle sim;
  Waveform.print ~display_width:100 waves
;;

let%expect_test "write_one_test" =
  testbench (fun sim inputs outputs ->
    inputs.clear := Bits.one 1;
    Cyclesim.cycle sim;
    inputs.clear := Bits.zero 1;
    inputs.write_data := Bits.of_int ~width:4 5;
    inputs.write_enable := Bits.one 1;
    inputs.pop_enable := Bits.zero 1;
    inputs.decrement := Bits.zero 1;
    Cyclesim.cycle sim;
    Printf.printf "Output: %d\n" (Bits.to_int !(outputs.front_val));
    inputs.write_data := Bits.of_int ~width:4 6;
    inputs.pop_enable := Bits.one 1;
    Cyclesim.cycle sim;
    inputs.write_enable := Bits.zero 1;
    inputs.pop_enable := Bits.zero 1;
    Printf.printf "Output: %d\n" (Bits.to_int !(outputs.front_val));
    Cyclesim.cycle sim;
    Printf.printf "Output: %d\n" (Bits.to_int !(outputs.front_val));
    Cyclesim.cycle sim;
    Printf.printf "Output: %d\n" (Bits.to_int !(outputs.front_val));
    inputs.decrement := Bits.one 1;
    Cyclesim.cycle sim;
    Printf.printf "Output: %d\n" (Bits.to_int !(outputs.front_val));
    Cyclesim.cycle sim;
    Printf.printf "Output: %d\n" (Bits.to_int !(outputs.front_val));
    ());
  [%expect
    {|
    Output: 5
    Output: 6
    Output: 6
    Output: 6
    Output: 5
    Output: 4
    ┌Signals───────────┐┌Waves─────────────────────────────────────────────────────────────────────────┐
    │clock             ││┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐ │
    │                  ││    └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   └─│
    │clear             ││────────┐                                                                     │
    │                  ││        └───────────────────────────────────────────────────────              │
    │decrement         ││                                        ┌───────────────────────              │
    │                  ││────────────────────────────────────────┘                                     │
    │pop_enable        ││                ┌───────┐                                                     │
    │                  ││────────────────┘       └───────────────────────────────────────              │
    │                  ││────────┬───────┬───────────────────────────────────────────────              │
    │write_data        ││ 0      │5      │6                                                            │
    │                  ││────────┴───────┴───────────────────────────────────────────────              │
    │write_enable      ││        ┌───────────────┐                                                     │
    │                  ││────────┘               └───────────────────────────────────────              │
    │                  ││────────────────┬───────┬───────────────────────┬───────┬───────              │
    │front_val         ││ 0              │5      │6                      │5      │4                    │
    │                  ││────────────────┴───────┴───────────────────────┴───────┴───────              │
    │                  ││                                                                              │
    │                  ││                                                                              │
    └──────────────────┘└──────────────────────────────────────────────────────────────────────────────┘
    |}]
;;
