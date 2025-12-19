open Hardcaml
open Signal

module I : sig
  type 'a t =
    { clock : 'a (* ; clear : 'a *)
    ; clear : 'a
    ; write_data : 'a [@bits 4] (*4 bits so each position can store at least 0-9*)
    ; write_enable : 'a [@bits 1]
    ; pop_enable : 'a
          [@bits 1] (*This doesn't really enable the output, its asynchronous read*)
    ; decrement : 'a [@bits 1]
    }
  [@@deriving hardcaml]
end

module O : sig
  type 'a t = { front_val : 'a [@bits 4] } [@@deriving hardcaml]
end

val hierarchical : Scope.t -> string -> t I.t -> t O.t
