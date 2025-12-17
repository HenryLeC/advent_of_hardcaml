open Hardcaml

val create_circuit : int -> int -> Circuit.t

val sub_counter_reg
  :  Reg_spec.t
  -> Reg_spec.signal
  -> Reg_spec.signal
  -> Reg_spec.signal
  -> Reg_spec.signal * Reg_spec.signal * Reg_spec.signal
