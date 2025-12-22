open Hardcaml
open Signal

module I = struct
  type 'a t =
    { clock : 'a (* ; clear : 'a *)
    ; write_lsp : 'a [@bits 4] (*4 bits so each position can store at least 0-9*)
    ; write_mask : 'a
          [@bits 12]
          (*Mask where each bit sets write enable, lsb writes lsb and other bits cause a shift*)
    ; read_idx : 'a [@bits 4]
    }
  [@@deriving hardcaml]
end

module O = struct
  type 'a t =
    { read_val : 'a [@bits 4]
    ; read_prev_val : 'a [@bits 4]
    }
  [@@deriving hardcaml]
end

let bank_mem instance (_scope : Scope.t) (inputs : Signal.t I.t) =
  let size = 12 in
  let spec = Reg_spec.create () ~clock:inputs.clock in
  let update_lines =
    Array.init size
    @@ fun idx -> Signal.wire 4 -- Printf.sprintf "%s update_line: %i" instance idx
  in
  let mem =
    Array.init size
    @@ fun idx ->
    Signal.reg
      spec
      update_lines.(idx)
      ~enable:(select inputs.write_mask (11 - idx) (11 - idx))
  in
  Signal.(update_lines.(11) <== inputs.write_lsp);
  for idx = 0 to size - 2 do
    update_lines.(idx) <== mem.(idx + 1)
  done;
  let read_line = Signal.wire 4 in
  let read_line_prev = Signal.wire 4 in
  let mem_list = Array.to_list mem in
  read_line <== mux inputs.read_idx mem_list;
  read_line_prev <== mux (inputs.read_idx -:. 1) mem_list;
  { O.read_val = read_line; read_prev_val = read_line_prev }
;;

let hierarchical (scope : Scope.t) instance (input : Signal.t I.t) =
  let module H = Hierarchy.In_scope (I) (O) in
  H.hierarchical ~scope ~name:"bank_mem" ~instance (bank_mem instance) input
;;
