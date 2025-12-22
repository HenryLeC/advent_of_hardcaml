open Hardcaml
open Signal

module I : sig
  type 'a t =
    { clock : 'a
    ; start : 'a
    ; finish : 'a
    ; clear : 'a
    ; digit : 'a [@bits 4]
    }
  [@@deriving hardcaml]
end

module O : sig
  type 'a t = { value : 'a [@bits 32] } [@@deriving hardcaml]
end

val hierarchical : Scope.t -> string -> t I.t -> t O.t
