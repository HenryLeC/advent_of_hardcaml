open! Hardcaml

module States = struct
  type t =
    | Wait_for_start
    | Process_stream
    | Wait_for_div
    | Process_results
    | Output
  [@@deriving sexp_of, compare ~localize, enumerate]
end

let advent_soln spec first value finish =
  let open Signal in
  let sm = Always.State_machine.create (module States) ~enable:vdd spec in
  let _ = sm.current -- "current_state" in
  let passes = Always.Variable.wire ~default:(Signal.zero 16) in
  let _ = passes.value -- "passes" in
  let start_div_mod = Always.Variable.wire ~default:Signal.gnd in
  let _ = start_div_mod.value -- "start_div_mod" in
  let zero_count = Always.Variable.reg spec ~enable:vdd ~width:16 in
  let position = Always.Variable.reg spec ~enable:vdd ~width:16 in
  let ready = Always.Variable.wire ~default:gnd in
  (* Next location modulo 100 computer *)
  let next_location, _, next_location_valid =
    Div_mod.sub_counter_reg
      spec
      start_div_mod.value
      (position.value +: value)
      (Signal.of_int 100 ~width:16)
  in
  (* Offset div mod 100 computer *)
  let offset_mod, offset_div, offset_dm_valid =
    Div_mod.sub_counter_reg spec start_div_mod.value value (Signal.of_int 100 ~width:16)
  in
  let _ = offset_div -- "offset_div" in
  Always.(
    compile
      [ sm.switch
          [ ( Wait_for_start
            , [ when_
                  first
                  [ sm.set_next Process_stream; position <--. 50; ready <-- vdd ]
              ] )
          ; ( Process_stream
            , [ ready <-- vdd
              ; if_
                  finish
                  [ sm.set_next Output ]
                  [ start_div_mod <-- vdd; sm.set_next Wait_for_div ]
              ] )
          ; ( Wait_for_div
            , [ when_
                  (next_location_valid &: offset_dm_valid)
                  [ sm.set_next Process_results ]
              ] )
          ; ( Process_results
            , [ if_ finish [ sm.set_next Output ] [ sm.set_next Process_stream ]
              ; if_
                  (value >+. 0)
                  [ (let position_offset =
                       position.value +: value -- "position_offset_pos"
                     in
                     if_
                       (position_offset <+. 100)
                       [ passes <-- of_int ~width:16 0 ]
                       [ (let inc =
                            mux2
                              (position.value +: offset_mod >=+. 100)
                              (of_int ~width:16 1)
                              (of_int ~width:16 0)
                          in
                          passes <-- offset_div +: inc)
                       ])
                  ]
                  [ (let position_offset =
                       position.value +: value -- "position_offset_neg"
                     in
                     if_
                       (position_offset >+. 0)
                       [ passes <-- of_int ~width:16 0 ]
                       [ (let loc =
                            mux2
                              (position.value ==:. 0)
                              (of_int ~width:16 100)
                              position.value
                          in
                          let inc =
                            mux2
                              (loc +: offset_mod -:. 100 <=+. 0)
                              (of_int ~width:16 1)
                              (of_int ~width:16 0)
                            -- "inc_neg"
                          in
                          (* The minus 1 is because of how the calculation supports negatives*)
                          passes <-- offset_div -:. 1 +: inc)
                       ])
                  ]
              ; zero_count <-- zero_count.value +: passes.value
              ; position <-- next_location
              ] )
          ; Output, []
          ]
      ]);
  ( output "position" position.value
  , output "zero_count" zero_count.value
  , output "ready" ready.value )
;;

let create_circuit width_in =
  let open Signal in
  let clock = input "clock" 1 in
  let first = input "first" 1 in
  let value = input "value" width_in in
  let finish = input "finish" 1 in
  let spec = Reg_spec.create ~clock () in
  let position, zero_count, ready = advent_soln spec first value finish in
  Circuit.create_exn
    ~name:"advent_soln"
    [ output "position" position; output "zero_count" zero_count; output "ready" ready ]
;;
