open! Core
open! Hardcaml
open! Hardcaml_waveterm
open! Bits
module Waveform = Hardcaml_waveterm.Waveform
module Advent_soln = Hardcaml_day1.Advent_soln
(* module Harness = Cyclesim_harness.Make (Range_finder.I) (Range_finder.O) *)

let sample_input_values = [ -68; -30; 48; -5; 60; -55; -1; -99; 14; -82 ]

let create_sim circuit =
  let sim = Cyclesim.create ~config:Cyclesim.Config.trace_all circuit in
  let waves, sim = Waveform.create sim in
  let first = Cyclesim.in_port sim "first" in
  let value = Cyclesim.in_port sim "value" in
  let finish = Cyclesim.in_port sim "finish" in
  let position = Cyclesim.out_port sim "position" in
  let zero_count = Cyclesim.out_port sim "zero_count" in
  let ready = Cyclesim.out_port sim "ready" in
  waves, sim, first, value, finish, position, zero_count, ready
;;

let test () =
  let open Bits in
  let waves, sim, first, value, finish, position, zero_count, ready =
    Advent_soln.(create_circuit) 16 |> create_sim
  in
  let filename = "./waves.vcd" in
  let oc = Out_channel.create filename in
  let sim = Vcd.wrap oc sim in
  let cycle () = Cyclesim.cycle sim in
  let feed_input n =
    value := n;
    cycle ();
    while Bits.to_bool !ready |> not do
      cycle ()
    done
  in
  first := vdd;
  cycle ();
  first := gnd;
  (* cycle (); *)
  List.iter sample_input_values ~f:(fun x -> feed_input @@ of_int ~width:16 x);
  finish := vdd;
  cycle ();
  cycle ();
  (* Closing the out channel will ensure the file is flushed to disk *)
  Out_channel.close oc;
  waves, !position, !zero_count
;;

let waves, position, zero_count = test ();;

Stdio.printf
  "position: %i zero_count: %i\n"
  (Bits.to_int position)
  (Bits.to_int zero_count)
;;

Waveform.print ~wave_width:1 waves
