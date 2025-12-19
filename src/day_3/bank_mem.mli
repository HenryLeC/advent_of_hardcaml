open Hardcaml
open Signal

module I : sig
  type 'a t =
    { clock : 'a (* ; clear : 'a *)
    ; write_lsp : 'a [@bits 4]
    ; write_mask : 'a [@bits 12]
    ; read_idx : 'a [@bits 4]
    }
  [@@deriving hardcaml]
end

module O : sig
  type 'a t =
    { read_val : 'a [@bits 4]
    ; read_prev_val : 'a [@bits 4]
    }
  [@@deriving hardcaml]
end

val hierarchical : Scope.t -> string -> t I.t -> t O.t
