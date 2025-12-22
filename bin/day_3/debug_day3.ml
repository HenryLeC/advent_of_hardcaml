open Hardcaml
module Calculate_joltage = Hardcaml_day3.Calculate_joltage
module Sim = Cyclesim.With_interface (Calculate_joltage.I) (Calculate_joltage.O)
module Waveform = Hardcaml_waveterm.Waveform
open Hardcaml_waveterm

let () =
  let scope = Scope.create ~flatten_design:true () in
  let sim =
    Sim.create
      ~config:(Cyclesim.Config.trace `Everything)
      (Calculate_joltage.calculate_joltage scope)
  in
  let waves, sim = Waveform.create sim in
  (* Run simulation *)
  for _ = 0 to 100 do
    Cyclesim.cycle sim
  done;
  (* Launch interactive viewer *)
  Hardcaml_waveterm_interactive.run ~wave_width:5 ~signals_width:30 waves
;;
