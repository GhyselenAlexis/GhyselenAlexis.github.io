#use "MAB_Abstract.ml" ;;

(* 
#use "RL_Naive.ml";; 

with (mab_handler 6) handle 
with (abs_MAB 6) handle 
with (ia_pgreedy_estimate 0.00 20.) handle 
let rec run n r = 
  if n = 0 then printestimation ();r else   
    let a = choice () in (do a);
    let r' = observe () in 
    reward r'; 
    run (n-1) (r +. r')
  in run 500 0. 
;;
*)
(*
#use "RL_Naive_Nonstationnary.ml" ;;

with (mab_handler 6) handle 
with (abs_MAB 6) handle 
with (ia_pgreedy_estimate 0.00 20. 0.1) handle 
let rec run n r = 
  if n = 0 then printestimation ();r else   
    let a = choice () in (do a);
    let r' = observe () in 
    reward r'; 
    run (n-1) (r +. r')
  in run 500 0. 
;; *)

#use "RL_QLearning.ml";; 

with (mab_handler 6 ) handle 
with (abs_MAB 6) handle 
with (ia_qlearning 0.1 10. 0.01 0.01) handle 
let rec run n r = 
  if n = 0 then printestimation ();r else   
    let a = choice () in (do a);
    let r' = observe () in 
    reward r'; 
    run (n-1) (r +. r')
  in run 0 0. 
;;