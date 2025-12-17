open! Core
open! Hardcaml
open! Bits
open! Signal

let partial_sub (a : Signal.t) (b : Signal.t) =
  let res = mux2 (a <+. 0) (a +: b) (a -: b) in
  mux2 (a <+ b &: (a >=+. 0)) a res
;;

let sub_counter_reg spec first a b =
  let width_a = width a in
  let modulo_w = wire width_a -- "running_modulo" in
  let mod_or_a = mux2 first a modulo_w in
  let divisor_w = wire width_a -- "running_divisor" in
  let divisor_w_or_0 = mux2 first (Signal.of_int ~width:width_a 0) divisor_w in
  let modulo = partial_sub mod_or_a b -- "modulo_next" in
  let modulo_r = reg spec ~enable:vdd modulo in
  let divisor_r =
    reg spec (mux2 (modulo <>: mod_or_a) (divisor_w_or_0 +:. 1) divisor_w_or_0)
  in
  let valid_r = reg spec (mux2 (modulo <>: mod_or_a) gnd vdd) in
  modulo_w <== modulo_r;
  divisor_w <== divisor_r;
  modulo_r, divisor_r, valid_r
;;

let create_circuit a_width b_width =
  let clock = input "clock" 1 in
  let first = input "first" 1 in
  let a = input "a" a_width in
  let b = input "b" b_width in
  let spec = Reg_spec.create ~clock () in
  let modulo, divisor, valid = sub_counter_reg spec first a b in
  Circuit.create_exn
    ~name:"div_mod"
    [ output "modulo" modulo; output "divisor" divisor; output "valid" valid ]
;;
