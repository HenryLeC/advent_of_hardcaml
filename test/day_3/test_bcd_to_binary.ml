open! Core
open! Hardcaml
open! Bits
module Waveform = Hardcaml_waveterm.Waveform
module Bcd_to_binary = Hardcaml_day3.Bcd_to_binary
module Sim = Cyclesim.With_interface (Bcd_to_binary.I) (Bcd_to_binary.O)

let testbench (test : Sim.t -> t ref Bcd_to_binary.I.t -> t ref Bcd_to_binary.O.t -> unit)
  =
  let scope = Scope.create ~flatten_design:true () in
  let sim = Sim.create (Bcd_to_binary.hierarchical scope "test_instance") in
  let waves, sim = Waveform.create sim in
  let inputs = Cyclesim.inputs sim in
  let outputs = Cyclesim.outputs sim in
  test sim inputs outputs;
  Cyclesim.cycle sim;
  let open Hardcaml_waveterm in
  Waveform.print
    ~display_width:85
    ~display_height:14
    ~display_rules:
      Display_rule.
        [ port_name_is "value" ~wave_format:Unsigned_int
        ; port_name_matches Re.Posix.(compile (re ".*")) ~wave_format:(Bit_or Hex)
        ]
    waves
;;

let%expect_test "write_one_test" =
  testbench (fun sim inputs outputs ->
    inputs.clear := vdd;
    Cyclesim.cycle sim;
    inputs.start := vdd;
    inputs.digit := Bits.of_int ~width:4 1;
    Cyclesim.cycle sim;
    inputs.start := gnd;
    inputs.digit := Bits.of_int ~width:4 2;
    Cyclesim.cycle sim;
    inputs.digit := Bits.of_int ~width:4 3;
    inputs.finish := vdd;
    Cyclesim.cycle sim;
    Printf.printf "Output: %d\n" (Bits.to_int !(outputs.value));
    inputs.start := vdd;
    inputs.finish := gnd;
    inputs.digit := Bits.of_int ~width:4 2;
    Cyclesim.cycle sim;
    inputs.start := gnd;
    inputs.digit := Bits.of_int ~width:4 4;
    Cyclesim.cycle sim;
    inputs.digit := Bits.of_int ~width:4 5;
    inputs.finish := vdd;
    Cyclesim.cycle sim;
    Printf.printf "Output: %d\n" (Bits.to_int !(outputs.value));
    ());
  [%expect
    {|
    Output: 123
    Output: 245
    ┌Signals───────────┐┌Waves──────────────────────────────────────────────────────────┐
    │                  ││────────────────┬───────┬───────┬───────┬───────┬───────┬──────│
    │value             ││ 0              │1      │12     │123    │2      │24     │245   │
    │                  ││────────────────┴───────┴───────┴───────┴───────┴───────┴──────│
    │clock             ││┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐  │
    │                  ││    └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   └──│
    │                  ││────────┬───────┬───────┬───────┬───────┬───────┬──────────────│
    │digit             ││ 0      │1      │2      │3      │2      │4      │5             │
    │                  ││────────┴───────┴───────┴───────┴───────┴───────┴──────────────│
    │finish            ││                        ┌───────┐               ┌──────────────│
    │                  ││────────────────────────┘       └───────────────┘              │
    │start             ││        ┌───────┐               ┌───────┐                      │
    │                  ││────────┘       └───────────────┘       └──────────────────────│
    └──────────────────┘└───────────────────────────────────────────────────────────────┘
    |}]
;;
