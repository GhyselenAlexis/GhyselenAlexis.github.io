#use "MABStates_Abstract.ml" ;;
(* 
#use "RL_Naive.ml";; 

with (mabs_handler 6 2) handle 
with (abs_MABS 6) handle 
with (ia_pgreedy_estimate 0.00 50.) handle 
let rec run n r = 
  if n = 0 then printestimation ();r else   
    let a = choice () in (do a);
    let (r',o) = observe () in 
    reward r'; 
    run (n-1) (r +. r')
  in run 1000 0. 
;;
*)

(*
#use "RL_Naive_Nonstationnary.ml";; 

with (mabs_handler 6 2) handle 
with (abs_MABS 6) handle 
with (ia_pgreedy_estimate 0.00 50. 0.1) handle 
let rec run n r = 
  if n = 0 then printestimation ();r else   
    let a = choice () in (do a);
    let (r',o) = observe () in 
    reward r'; 
    run (n-1) (r +. r')
  in run 1000 0. 
;;
*) 


(*
#use "RL_Gradient_Learning.ml";; 

with (mabs_handler 6 2) handle 
with (abs_MABS 6) handle 
with (ia_gradient 0.1 10.) handle 
let rec run n r = 
  if n = 0 then printestimation ();r else   
    let a = choice () in (do a);
    let (r',o) = observe () in 
    reward r'; 
    run (n-1) (r +. r')
  in run 1000 0. 
;;
*) 


#use "RL_QLearning.ml";; 

with (mabs_handler 6 2) handle 
with (abs_MABS 6) handle 
with (ia_qlearning 0.1 10. 0.01 0.01) handle 
let rec run n r = 
  if n = 0 then printestimation ();r else   
    let a = choice () in (do a);
    let (r',o) = observe () in 
    reward r'; 
    run (n-1) (r +. r')
  in run 3000 0. 
;;



