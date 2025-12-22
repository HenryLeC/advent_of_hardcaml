open! Core
open! Hardcaml
open! Bits
module Waveform = Hardcaml_waveterm.Waveform
module Day3_soln = Hardcaml_day3.Day3_soln
module Sim = Cyclesim.With_interface (Day3_soln.I) (Day3_soln.O)

let testbench (test : Sim.t -> t ref Day3_soln.I.t -> t ref Day3_soln.O.t -> unit) =
  let scope = Scope.create ~flatten_design:true () in
  let sim =
    Sim.create
      ~config:(Cyclesim.Config.trace `Everything)
      (Day3_soln.hierarchical scope "test_soln")
  in
  let waves, sim = Waveform.create sim in
  let out = Out_channel.create "/home/henrylec/day3.vcd" in
  let sim = Vcd.wrap out sim in
  let inputs = Cyclesim.inputs sim in
  let outputs = Cyclesim.outputs sim in
  test sim inputs outputs;
  while Bits.to_bool !(outputs.valid) |> not do
    Cyclesim.cycle sim
  done;
  let open Hardcaml_waveterm in
  Waveform.print ~display_width:100 waves;
  Out_channel.close out
;;

let explode s =
  let rec expl i l = if i < 0 then l else expl (i - 1) (s.[i] :: l) in
  expl (String.length s - 1) []
;;

let test_data data =
  let data = explode data in
  testbench (fun sim inputs outputs ->
    let feed_data =
      fun len idx char ->
      inputs.char_data := Bits.of_int ~width:8 @@ Stdlib.Char.code char;
      inputs.finish := if idx = len - 1 then vdd else gnd;
      Cyclesim.cycle sim;
      while Bits.to_bool !(outputs.ready) |> not do
        Cyclesim.cycle sim
      done;
      if idx mod 101 = 5 then Printf.printf "%d\n" @@ Bits.to_int !(outputs.output)
    in
    (* inputs.reset := vdd;
    Cyclesim.cycle sim; *)
    inputs.reset := gnd;
    inputs.start := vdd;
    Cyclesim.cycle sim;
    List.iteri data ~f:(feed_data @@ List.length data);
    inputs.finish := vdd;
    Cyclesim.cycle sim;
    while Bits.to_bool !(outputs.valid) |> not do
      Cyclesim.cycle sim
    done;
    Cyclesim.cycle sim;
    Printf.printf "output: %d\n" @@ Bits.to_int !(outputs.output);
    ())
;;

let read_file filename =
  let ic = In_channel.create filename in
  let rec create ic =
    let line = In_channel.input_line ic in
    match line with
    | Some line -> line ^ "\n" ^ create ic
    | None -> ""
  in
  create ic
;;

let%expect_test "big_data_1" =
  let data_str = read_file "../../../../../../inputs/day3.txt" in
  test_data data_str;
  [%expect
    {|
    output: 169077317650745
    ┌Signals───────────┐┌Waves─────────────────────────────────────────────────────────────────────────┐
    │clock             ││┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐ │
    │                  ││    └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   └─│
    │reset             ││                                                                              │
    │                  ││──────────────────────────────────────────────────────────────────────────────│
    │                  ││────────┬───────────────┬───────┬───────┬───────┬───────┬───────┬─────────────│
    │char_data         ││ 00     │34             │32     │36     │35     │34     │36     │35           │
    │                  ││────────┴───────────────┴───────┴───────┴───────┴───────┴───────┴─────────────│
    │finish            ││                                                                              │
    │                  ││──────────────────────────────────────────────────────────────────────────────│
    │start             ││──────────────────────────────────────────────────────────────────────────────│
    │                  ││                                                                              │
    │                  ││──────────────────────────────────────────────────────────────────────────────│
    │output            ││ 0000000000000000                                                             │
    │                  ││──────────────────────────────────────────────────────────────────────────────│
    │ready             ││──────────────────────────────────────────────────────────────────────────────│
    │                  ││                                                                              │
    │valid             ││                                                                              │
    │                  ││──────────────────────────────────────────────────────────────────────────────│
    └──────────────────┘└──────────────────────────────────────────────────────────────────────────────┘
    |}]
;;
