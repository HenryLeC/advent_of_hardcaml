open! Core
open! Hardcaml
open! Bits
module Waveform = Hardcaml_waveterm.Waveform
module Div_mod = Hardcaml_day1.Div_mod
(* module Harness = Cyclesim_harness.Make (Range_finder.I) (Range_finder.O) *)

let create_sim circuit =
  let sim = Cyclesim.create ~config:Cyclesim.Config.trace_all circuit in
  let waves, sim = Waveform.create sim in
  let first = Cyclesim.in_port sim "first" in
  let a = Cyclesim.in_port sim "a" in
  let b = Cyclesim.in_port sim "b" in
  let modulo = Cyclesim.out_port sim "modulo" in
  let divisor = Cyclesim.out_port sim "divisor" in
  let valid = Cyclesim.out_port sim "valid" in
  waves, sim, a, b, first, modulo, divisor, valid
;;

let test a_in b_in =
  let open Bits in
  let waves, sim, a, b, first, modulo, divisor, valid =
    Div_mod.(create_circuit) (width a_in) (width b_in) |> create_sim
  in
  let step iteration =
    first := if iteration = 0 then vdd else gnd;
    Cyclesim.cycle sim
  in
  a := a_in;
  b := b_in;
  for i = 0 to width b_in - 1 do
    step i
  done;
  let modulo = !modulo in
  let divisor = !divisor in
  Cyclesim.cycle sim;
  waves, modulo, divisor, !valid
;;

let waves, modulo, divisor, valid = test (of_int ~width:5 7) (Bits.of_int ~width:5 3);;

Stdio.printf
  "modulo: %i divisor: %i valid %b\n"
  (Bits.to_sint modulo)
  (Bits.to_sint divisor)
  (Bits.to_bool valid)
;;

Waveform.print waves
