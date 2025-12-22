open Hardcaml
open Signal

module I = struct
  type 'a t =
    { clock : 'a
    ; reset : 'a
    ; start : 'a
    ; finish : 'a
    ; char_data : 'a [@bits 8]
    }
  [@@deriving hardcaml]
end

module O = struct
  type 'a t =
    { ready : 'a
    ; valid : 'a
    ; output : 'a [@bits 64]
    }
  [@@deriving hardcaml]
end

let ( -- ) (a : Always.Variable.t) (name : string) : Always.Variable.t =
  let _ = a.value -- name in
  a
;;

module States = struct
  type t =
    | Wait_for_start
    | Process_line
    | Wait_for_result
    | Reset_calc
  [@@deriving sexp_of, compare ~localize, enumerate]
end

let day3_soln (_scope : Scope.t) (inputs : t I.t) =
  let spec = Reg_spec.create ~clock:inputs.clock ~clear:inputs.reset () in
  let sm = Always.State_machine.create (module States) spec in
  let prev_char = Always.Variable.reg ~width:8 spec -- "prev_char" in
  let result = Always.Variable.reg ~width:64 spec in
  let calc_reset = Always.Variable.wire ~default:gnd -- "cal_reset" in
  let calc_start = Always.Variable.reg ~width:1 spec -- "calc_start" in
  let calc_finish = Always.Variable.wire ~default:gnd -- "calc_finish" in
  let calc_inputs =
    { Calculate_joltage.I.clock = inputs.clock
    ; reset = calc_reset.value
    ; start = calc_start.value
    ; finish = calc_finish.value
    ; char_data = (prev_char.value -:. Char.code '0' |> uresize) 4
    }
  in
  let calc = Calculate_joltage.hierarchical _scope "calculate_joltage" calc_inputs in
  let _ = Signal.( -- ) calc.valid "calc_valid" in
  let _ = Signal.( -- ) calc.state "calc_state" in
  let ready = Always.Variable.wire ~default:gnd in
  let valid = Always.Variable.reg ~width:1 spec in
  Always.(
    compile
      [ sm.switch
          [ ( Wait_for_start
            , [ ready <-- vdd
              ; when_
                  (inputs.start &: ready.value)
                  [ sm.set_next Process_line
                  ; prev_char <-- inputs.char_data
                  ; calc_start <-- vdd
                  ]
              ] )
          ; ( Process_line
            , [ calc_start <-- gnd
              ; when_ calc.ready []
              ; prev_char <-- inputs.char_data
              ; ready <-- calc.ready
              ; when_
                  (inputs.char_data ==:. Char.code '\n')
                  [ sm.set_next Wait_for_result; calc_finish <-- vdd ]
              ] )
          ; ( Wait_for_result
            , [ calc_finish <-- vdd
              ; when_
                  calc.valid
                  [ result <-- result.value +: uresize calc.joltage 64
                  ; if_
                      inputs.finish
                      [ sm.set_next Reset_calc; valid <-- vdd ]
                      [ sm.set_next Reset_calc
                      ; prev_char <-- inputs.char_data
                      ; calc_start <-- vdd
                      ; ready <-- vdd
                      ; calc_reset <-- vdd
                      ]
                  ]
              ] )
          ; Reset_calc, [ sm.set_next Wait_for_start; calc_start <-- vdd ]
          ]
      ]);
  { O.output = result.value; ready = ready.value; valid = valid.value }
;;

let hierarchical (scope : Scope.t) instance (input : Signal.t I.t) =
  let module H = Hierarchy.In_scope (I) (O) in
  H.hierarchical ~scope ~name:"day3_soln" ~instance day3_soln input
;;
