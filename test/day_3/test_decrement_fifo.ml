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
  let sim =
    Sim.create
      ~config:(Cyclesim.Config.trace `All_named)
      (Decrement_fifo.hierarchical scope "test_instance")
  in
  let waves, sim = Waveform.create sim in
  let out = Out_channel.create "../../../../../../out/fifo.vcd" in
  let sim = Vcd.wrap out sim in
  let inputs = Cyclesim.inputs sim in
  let outputs = Cyclesim.outputs sim in
  test sim inputs outputs;
  Cyclesim.cycle sim;
  Out_channel.close out;
  Waveform.print ~display_width:100 ~display_height:30 waves
;;

let%expect_test "write_one_test" =
  testbench (fun sim inputs outputs ->
    inputs.clear := vdd;
    Cyclesim.cycle sim;
    inputs.clear := gnd;
    inputs.write_data := Bits.of_int ~width:4 5;
    inputs.write_enable := vdd;
    inputs.pop_enable := gnd;
    inputs.decrement := gnd;
    Cyclesim.cycle sim;
    Printf.printf "Output: %d\n" (Bits.to_int !(outputs.front_val));
    inputs.write_data := Bits.of_int ~width:4 6;
    Cyclesim.cycle sim;
    Printf.printf "Output: %d\n" (Bits.to_int !(outputs.front_val));
    inputs.write_data := Bits.of_int ~width:4 7;
    Cyclesim.cycle sim;
    Printf.printf "Output: %d\n" (Bits.to_int !(outputs.front_val));
    inputs.write_data := Bits.of_int ~width:4 8;
    Cyclesim.cycle sim;
    inputs.pop_enable := vdd;
    inputs.write_enable := gnd;
    Printf.printf "Output: %d\n" (Bits.to_int !(outputs.front_val));
    Cyclesim.cycle sim;
    inputs.pop_enable := gnd;
    Printf.printf "Output: %d\n" (Bits.to_int !(outputs.front_val));
    Cyclesim.cycle sim;
    Printf.printf "Output: %d\n" (Bits.to_int !(outputs.front_val));
    inputs.decrement := vdd;
    Cyclesim.cycle sim;
    Printf.printf "Output: %d\n" (Bits.to_int !(outputs.front_val));
    Cyclesim.cycle sim;
    Printf.printf "Output: %d\n" (Bits.to_int !(outputs.front_val));
    ());
  [%expect
    {|
    Output: 5
    Output: 5
    Output: 5
    Output: 5
    Output: 6
    Output: 6
    Output: 5
    Output: 4
    ┌Signals───────────┐┌Waves─────────────────────────────────────────────────────────────────────────┐
    │clock             ││┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐ │
    │                  ││    └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   └─│
    │clear             ││────────┐                                                                     │
    │                  ││        └─────────────────────────────────────────────────────────────────────│
    │decrement         ││                                                        ┌─────────────────────│
    │                  ││────────────────────────────────────────────────────────┘                     │
    │pop_enable        ││                                        ┌───────┐                             │
    │                  ││────────────────────────────────────────┘       └─────────────────────────────│
    │                  ││────────┬───────┬───────┬───────┬─────────────────────────────────────────────│
    │write_data        ││ 0      │5      │6      │7      │8                                            │
    │                  ││────────┴───────┴───────┴───────┴─────────────────────────────────────────────│
    │write_enable      ││        ┌───────────────────────────────┐                                     │
    │                  ││────────┘                               └─────────────────────────────────────│
    │                  ││────────────────┬───────┬───────┬───────┬─────────────────────────────────────│
    │back_idx          ││ 0              │1      │2      │3      │4                                    │
    │                  ││────────────────┴───────┴───────┴───────┴─────────────────────────────────────│
    │empty             ││────────────────┐                                                             │
    │                  ││                └─────────────────────────────────────────────────────────────│
    │                  ││────────────────────────────────────────────────┬─────────────────────────────│
    │front_idx         ││ 0                                              │1                            │
    │                  ││────────────────────────────────────────────────┴─────────────────────────────│
    │                  ││────────────────┬───────────────────────────────┬───────────────┬───────┬─────│
    │front_val         ││ 0              │5                              │6              │5      │4    │
    │                  ││────────────────┴───────────────────────────────┴───────────────┴───────┴─────│
    │                  ││────────┬───────────────────────────────────────────────┬───────┬───────┬─────│
    │update_line: 0    ││ 0      │5                                              │4      │3      │2    │
    │                  ││────────┴───────────────────────────────────────────────┴───────┴───────┴─────│
    │                  ││────────────────┬───────────────────────────────────────┬───────┬───────┬─────│
    └──────────────────┘└──────────────────────────────────────────────────────────────────────────────┘
    |}]
;;
