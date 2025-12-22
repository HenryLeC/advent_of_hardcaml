open Hardcaml
open Signal

module I = struct
  type 'a t =
    { clock : 'a
    ; start : 'a
    ; finish : 'a
    ; clear : 'a
    ; digit : 'a [@bits 4]
    }
  [@@deriving hardcaml]
end

module O = struct
  type 'a t = { value : 'a [@bits 64] } [@@deriving hardcaml]
end

let create (_scope : Scope.t) (inputs : t I.t) =
  let spec = Reg_spec.create ~clock:inputs.clock () in
  (* Find the number of digits to convert and create a counter. *)
  let started = Always.Variable.reg spec ~width:1 in
  let acc = Always.Variable.reg spec ~width:64 in
  (* Select the current digit being processed. Note that when we split [bcd] into [digits]
     we did so from the top most digit. *)
  let digit = uresize inputs.digit (width acc.value) in
  Always.(
    compile
      [ if_
          ~:(started.value)
          [ (* Wait for start. When applied set the accumulator with the top most digit.
            *)
            when_ inputs.start [ started <--. 1; acc <-- digit ]
          ]
          [ (* Add each succesive digit to the accumulator times 10. *)
            acc <-- drop_top (acc.value *: Signal.of_int ~width:4 10) 4 +: digit
          ; (* Finished processing digits. *)
            when_ inputs.finish [ started <--. 0 ]
          ]
      ]);
  { O.value = acc.value }
;;

let hierarchical (scope : Scope.t) instance (inputs : t I.t) =
  let module H = Hierarchy.In_scope (I) (O) in
  H.hierarchical ~scope ~name:"bank_mem" ~instance create inputs
;;
