open Hardcaml
open Signal

module I = struct
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

module O = struct
  type 'a t =
    { front_val : 'a [@bits 4]
    ; empty : 'a
    ; front_idx : 'a [@bits 4]
    ; back_idx : 'a [@bits 4]
    }
  [@@deriving hardcaml]
end

let decrement_fifo (_scope : Scope.t) (inputs : t I.t) =
  let size = 12 in
  let spec = Reg_spec.create () ~clock:inputs.clock ~clear:inputs.clear in
  let front_idx =
    Signal.reg_fb
      spec
      ~enable:inputs.pop_enable
      ~f:(fun t ->
        let next = t +:. 1 in
        mux2 (next <:. size) next (Signal.of_int ~width:4 0))
      ~width:4
  in
  let back_idx =
    Signal.reg_fb
      spec
      ~enable:inputs.write_enable
      ~f:(fun t ->
        let next = t +:. 1 in
        mux2 (next <:. size) next (Signal.of_int ~width:4 0))
      ~width:4
  in
  let update_lines =
    Array.init size @@ fun idx -> Signal.wire 4 -- Printf.sprintf "update_line: %i" idx
  in
  let mem =
    Array.init size
    @@ fun idx ->
    Signal.reg spec update_lines.(idx) ~enable:(inputs.decrement ||: inputs.write_enable)
  in
  for idx = 0 to size - 1 do
    update_lines.(idx)
    <== mux2
          (inputs.write_enable &: (back_idx ==:. idx))
          (* If we are writing to the buffer and are one past the end *)
          inputs.write_data
          (* Put in the data to write *)
          (mux2 inputs.decrement (mem.(idx) -:. 1) mem.(idx))
    (* Else if we want to decrement decrement it or write back the current data *)
  done;
  let front_data = Signal.wire 4 in
  front_data <== mux front_idx @@ Array.to_list mem;
  { O.front_val = front_data; empty = back_idx ==: front_idx; front_idx; back_idx }
;;

let hierarchical (scope : Scope.t) instance (input : t I.t) =
  let module H = Hierarchy.In_scope (I) (O) in
  H.hierarchical ~scope ~name:"decrement_fifo" ~instance decrement_fifo input
;;
