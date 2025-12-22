open Hardcaml
open Signal

module I : sig
  type 'a t =
    { clock : 'a (* ; clear : 'a *)
    ; write_lsp : 'a [@bits 4]
    ; write_mask : 'a [@bits 12] (* LSB is write in to 11; MSBS shift more significant *)
    ; read_idx : 'a [@bits 4] (* 11 is most significant 0 is least significant *)
    }
  [@@deriving hardcaml]
end

module O : sig
  type 'a t =
    { read_val : 'a [@bits 4] (* Less significant position *)
    ; read_prev_val : 'a [@bits 4] (* More significant position *)
    }
  [@@deriving hardcaml]
end

val hierarchical : Scope.t -> string -> t I.t -> t O.t
