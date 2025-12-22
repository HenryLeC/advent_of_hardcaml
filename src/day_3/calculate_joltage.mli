open Hardcaml
open Signal

module I : sig
  type 'a t =
    { clock : 'a (* ; clear : 'a *)
    ; reset : 'a
    ; start : 'a
    ; finish : 'a
    ; char_data : 'a [@bits 8]
    }
  [@@deriving hardcaml]
end

module O : sig
  type 'a t =
    { joltage : 'a [@bits 32]
    ; ready : 'a
    ; valid : 'a
    ; state : 'a [@bits 3]
    }
  [@@deriving hardcaml]
end

val calculate_joltage : Scope.t -> t I.t -> t O.t
val hierarchical : Scope.t -> string -> t I.t -> t O.t
