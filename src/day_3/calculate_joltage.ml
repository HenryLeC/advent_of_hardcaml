open Hardcaml
open Signal

module I = struct
  type 'a t =
    { clock : 'a (* ; clear : 'a *)
    ; reset : 'a
    ; start : 'a
    ; finish : 'a
    ; char_data : 'a [@bits 4]
    }
  [@@deriving hardcaml]
end

module O = struct
  type 'a t =
    { joltage : 'a [@bits 64]
    ; ready : 'a
    ; valid : 'a
    ; state : 'a [@bits 3]
    }
  [@@deriving hardcaml]
end

module States = struct
  type t =
    | Wait_for_start
    | Prefill_buffer
    | Update_bank
    | Update_buffer
    | Convert_bcd
  [@@deriving sexp_of, compare ~localize, enumerate]
end

let calculate_joltage (scope : Scope.t) (inputs : t I.t) =
  let spec = Reg_spec.create ~clock:inputs.clock ~clear:inputs.reset () in
  let bank_read_idx = Always.Variable.wire ~default:(of_int ~width:4 11) in
  let _ = bank_read_idx.value -- "bank_read_idx" in
  let bank_write_data = Always.Variable.wire ~default:(zero 4) in
  let _ = bank_write_data.value -- "bank_write_data" in
  let bank_write_mask = Always.Variable.wire ~default:(zero 12) in
  let _ = bank_write_mask.value -- "bank_write_mask" in
  let bank_input =
    { Bank_mem.I.clock = inputs.clock
    ; read_idx = bank_read_idx.value
    ; write_lsp = bank_write_data.value
    ; write_mask = bank_write_mask.value
    }
  in
  let bank_mem = Bank_mem.hierarchical scope "joltage_bank" bank_input in
  let _ = bank_mem.read_val -- "bank_read_val" in
  let _ = bank_mem.read_prev_val -- "bank_read_prev_val" in
  let pairs_write_data = Always.Variable.wire ~default:(zero 4) in
  let _ = pairs_write_data.value -- "pairs_write_data" in
  let pairs_write_enable = Always.Variable.wire ~default:(zero 1) in
  let _ = pairs_write_enable.value -- "pairs_write_enable" in
  let pairs_pop = Always.Variable.wire ~default:(zero 1) in
  let _ = pairs_pop.value -- "pairs_pop" in
  let pairs_decrement = Always.Variable.wire ~default:(zero 1) in
  let _ = pairs_decrement.value -- "pairs_decrement" in
  let pairs_input =
    { Decrement_fifo.I.clock = inputs.clock
    ; clear = inputs.reset
    ; write_data = pairs_write_data.value
    ; write_enable = pairs_write_enable.value
    ; pop_enable = pairs_pop.value
    ; decrement = pairs_decrement.value
    }
  in
  let inc_pairs = Decrement_fifo.hierarchical scope "inc_pairs" pairs_input in
  let _ = inc_pairs.front_val -- "inc_pairs_front" in
  let prev = Always.Variable.reg spec ~width:4 ~enable:vdd in
  let bank_size = Always.Variable.reg spec ~width:4 ~enable:vdd in
  let _ = bank_size.value -- "bank_size" in
  let check_increasing = Always.Variable.reg spec ~width:1 ~enable:vdd in
  let _ = check_increasing.value -- "check_increasing" in
  let ready = Always.Variable.reg spec ~width:1 ~enable:vdd in
  let valid_out = Always.Variable.reg spec ~width:1 ~enable:vdd in
  let sm = Always.State_machine.create (module States) ~enable:vdd spec in
  let bcd_input =
    { Bcd_to_binary.I.clock = inputs.clock
    ; clear = inputs.start
    ; start = (sm.current ==:. 4 &: (bank_size.value ==:. 12)) -- "bcd_start"
    ; finish = (bank_size.value ==:. 1) -- "bcd_finish"
    ; digit = bank_mem.read_val -- "bcd_digit"
    }
  in
  let bcd_conv = Bcd_to_binary.hierarchical scope "joltage_bcd" bcd_input in
  Always.(
    compile
      [ sm.switch
          [ ( Wait_for_start
            , [ ready <-- vdd
              ; when_
                  (inputs.start &: ready.value)
                  [ bank_write_mask <-- ones 12
                  ; bank_write_data <-- inputs.char_data
                  ; prev <-- inputs.char_data
                  ; bank_size <-- one 4
                  ; sm.set_next Prefill_buffer
                  ]
              ] )
          ; ( Prefill_buffer
            , [ ready <-- vdd
              ; when_ (bank_size.value ==:. 11) [ sm.set_next Update_bank ]
              ; when_
                  (inputs.char_data >: prev.value)
                  [ pairs_write_data <-- bank_size.value; pairs_write_enable <-- vdd ]
              ; bank_write_mask <-- ones 12
              ; bank_write_data <-- inputs.char_data
              ; prev <-- inputs.char_data
              ; bank_size <-- bank_size.value +:. 1
              ] )
          ; ( Update_bank
            , [ ready <-- gnd
              ; sm.set_next Update_buffer
              ; bank_write_data
                <-- inputs.char_data (* Writing always involves pushing data*)
              ; bank_read_idx <-- of_int 11 ~width:4
              ; if_
                  (inc_pairs.empty -- "pairs_empty")
                  [ (* If the buffer is empty we just want to replace the last element *)
                    when_
                      (inputs.char_data >: bank_mem.read_val)
                      [ bank_write_mask <-- one 12 ]
                  ]
                  [ (* Otherwise we definitely want to shift our value into the end *)
                    (let mask = log_shift srl (ones 12) (inc_pairs.front_val -:. 1) in
                     bank_write_mask <-- mask)
                  ; pairs_decrement <-- vdd
                    (* But we might need to keep the front element of the buffer *)
                  ; check_increasing <-- vdd
                  ]
              ; if_
                  (inputs.char_data >: bank_mem.read_val)
                  [ pairs_write_data <-- of_int 11 ~width:4; pairs_write_enable <-- vdd ]
                  [ pairs_write_enable <-- gnd ]
              ] )
          ; ( Update_buffer
            , [ ready <-- vdd
              ; pairs_write_enable <-- gnd
              ; bank_read_idx <-- inc_pairs.front_val
              ; if_
                  inputs.finish
                  [ sm.set_next Convert_bcd ]
                  [ sm.set_next Update_bank; bank_size <--. 12 ]
              ; check_increasing <-- gnd
              ; when_
                  check_increasing.value
                  [ if_
                      (inc_pairs.front_val
                       >=:. 1
                       &: (bank_mem.read_val >: bank_mem.read_prev_val))
                      []
                      [ pairs_pop <-- vdd ]
                  ]
              ] )
          ; ( Convert_bcd
            , [ if_
                  (bank_size.value ==:. 0)
                  [ valid_out <-- vdd; sm.set_next Wait_for_start ]
                  [ bank_size <-- bank_size.value -:. 1
                  ; bank_read_idx <-- of_int ~width:4 12 -: bank_size.value
                  ]
              ] )
          ]
      ]);
  { O.joltage = bcd_conv.value
  ; ready = ready.value
  ; state = uresize sm.current 3
  ; valid = valid_out.value
  }
;;

let hierarchical (scope : Scope.t) instance (input : Signal.t I.t) =
  let module H = Hierarchy.In_scope (I) (O) in
  H.hierarchical ~scope ~name:"calculate_joltage" ~instance calculate_joltage input
;;
