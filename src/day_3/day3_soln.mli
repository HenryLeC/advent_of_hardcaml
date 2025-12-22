open Hardcaml
open Signal

module I : sig
  type 'a t =
    { clock : 'a
    ; reset : 'a
    ; start : 'a
    ; finish : 'a
    ; char_data : 'a [@bits 8]
    }
  [@@deriving hardcaml]
end

module O : sig
  type 'a t =
    { ready : 'a
    ; valid : 'a
    ; output : 'a [@bits 64]
    }
  [@@deriving hardcaml]
end

val hierarchical : Scope.t -> string -> t I.t -> t O.t
