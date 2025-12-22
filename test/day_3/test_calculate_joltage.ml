open! Core
open! Hardcaml
open! Bits
module Waveform = Hardcaml_waveterm.Waveform
module Calculate_joltage = Hardcaml_day3.Calculate_joltage
module Sim = Cyclesim.With_interface (Calculate_joltage.I) (Calculate_joltage.O)

let testbench
      (test : Sim.t -> t ref Calculate_joltage.I.t -> t ref Calculate_joltage.O.t -> unit)
  =
  let scope = Scope.create ~flatten_design:true () in
  let sim =
    Sim.create
      ~config:(Cyclesim.Config.trace `Everything)
      (Calculate_joltage.calculate_joltage scope)
  in
  let waves, sim = Waveform.create sim in
  let out = Out_channel.create "/home/henrylec/waves.vcd" in
  let sim = Vcd.wrap out sim in
  let inputs = Cyclesim.inputs sim in
  let outputs = Cyclesim.outputs sim in
  test sim inputs outputs;
  Cyclesim.cycle sim;
  let open Hardcaml_waveterm in
  Waveform.print ~display_width:100 waves;
  Out_channel.close out
;;

let explode s =
  let rec expl i l = if i < 0 then l else expl (i - 1) (s.[i] :: l) in
  expl (String.length s - 1) []
;;

let data_str_to_list (str : string) =
  let chars = explode str in
  let rec conv c l =
    match c with
    | [] -> l
    | char :: chs -> conv chs (l @ [ Stdlib.Char.code char - Stdlib.Char.code '0' ])
  in
  conv chars []
;;

let test_data data =
  let data = data_str_to_list data in
  testbench (fun sim inputs outputs ->
    let feed_data =
      fun len idx num ->
      inputs.char_data := Bits.of_int ~width:4 num;
      inputs.finish := if idx = len - 1 then vdd else gnd;
      Cyclesim.cycle sim;
      while Bits.to_bool !(outputs.ready) |> not do
        Cyclesim.cycle sim
      done
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
    Printf.printf "joltage: %d\n" @@ Bits.to_int !(outputs.joltage);
    ())
;;

let%expect_test "big_data_1" =
  let data_str =
    "3422222121231221222421222222121272211223124211222222214432222512422226462422122222221351154222226222"
  in
  test_data data_str;
  [%expect
    {|
    joltage: 766554226222
    ┌Signals───────────┐┌Waves─────────────────────────────────────────────────────────────────────────┐
    │clock             ││┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐ │
    │                  ││    └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   └─│
    │reset             ││                                                                              │
    │                  ││──────────────────────────────────────────────────────────────────────────────│
    │                  ││────────┬───────┬───────┬───────────────────────────────────────┬───────┬─────│
    │char_data         ││ 0      │3      │4      │2                                      │1      │2    │
    │                  ││────────┴───────┴───────┴───────────────────────────────────────┴───────┴─────│
    │finish            ││                                                                              │
    │                  ││──────────────────────────────────────────────────────────────────────────────│
    │start             ││──────────────────────────────────────────────────────────────────────────────│
    │                  ││                                                                              │
    │                  ││──────────────────────────────────────────────────────────────────────────────│
    │joltage           ││ 0000000000000000                                                             │
    │                  ││──────────────────────────────────────────────────────────────────────────────│
    │ready             ││        ┌─────────────────────────────────────────────────────────────────────│
    │                  ││────────┘                                                                     │
    │                  ││────────────────┬─────────────────────────────────────────────────────────────│
    │state             ││ 0              │1                                                            │
    └──────────────────┘└──────────────────────────────────────────────────────────────────────────────┘
    |}]
;;

let%expect_test "big_data_2" =
  let data_str =
    "3134343613223133354232233332661513531335333432332323336333332333333323334433432234333333333632313335"
  in
  test_data data_str;
  [%expect
    {|
    joltage: 666663313335
    ┌Signals───────────┐┌Waves─────────────────────────────────────────────────────────────────────────┐
    │clock             ││┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐ │
    │                  ││    └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   └─│
    │reset             ││                                                                              │
    │                  ││──────────────────────────────────────────────────────────────────────────────│
    │                  ││────────┬───────┬───────┬───────┬───────┬───────┬───────┬───────┬───────┬─────│
    │char_data         ││ 0      │3      │1      │3      │4      │3      │4      │3      │6      │1    │
    │                  ││────────┴───────┴───────┴───────┴───────┴───────┴───────┴───────┴───────┴─────│
    │finish            ││                                                                              │
    │                  ││──────────────────────────────────────────────────────────────────────────────│
    │start             ││──────────────────────────────────────────────────────────────────────────────│
    │                  ││                                                                              │
    │                  ││──────────────────────────────────────────────────────────────────────────────│
    │joltage           ││ 0000000000000000                                                             │
    │                  ││──────────────────────────────────────────────────────────────────────────────│
    │ready             ││        ┌─────────────────────────────────────────────────────────────────────│
    │                  ││────────┘                                                                     │
    │                  ││────────────────┬─────────────────────────────────────────────────────────────│
    │state             ││ 0              │1                                                            │
    └──────────────────┘└──────────────────────────────────────────────────────────────────────────────┘
    |}]
;;
