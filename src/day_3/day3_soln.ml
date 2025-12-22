(* open Hardcaml
open Signal

module I = struct
  type 'a t =
    { clock : 'a (* ; clear : 'a *)
    ; start: 'a
    ; finish: 'a
    ; char_data : 'a [@bits 8]
    }
  [@@deriving hardcaml]
end

module O = struct
  type 'a t =
    { read_val : 'a [@bits 4]
    }
  [@@deriving hardcaml]
end

let day3_soln (_scope: Scope.t) (inputs: t I.t) =
  let  *)
