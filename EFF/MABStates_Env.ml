#use "EFF_List.ml" ;;
#use "MABStates_Types.ml";;

(* A File for the Environment. Here, the user must specify how the algebraic operation interact with the environment*)

(* The "internal memory" of the environment (the comodel). 
Here, for multi-armed bandit with states, it only remembers which was the previous reward and the current state*)
type env_mem = float * int

(* The reward of the machine a. It depends on a and it is obviously random *)
let reward (a:act) s = 10. *. (float_of_int s) +. (float_of_int a) +. (randomfloat 10.) ;;

(*One possible handler for the environment, the max corresponds to the number of machines *)
(* The machine m has a uniform reward between m and (m+3) *)
let mabs_handler max nbs = handler
  | y -> (fun (_:env_mem) -> y)
  | effect (Do a) k -> fun (_,s) -> if (a > 0) && (a <= max) then begin 
      print ("You chosed Machine number " ^ (string_of_int a) ^ "\n"); (continue k ()) (reward a s,randomint nbs)
  end 
  else raise "This action is not available ! \n"
  | effect Observe k -> fun (r,s) -> (continue k (r,s)) (r,s)   
  | finally f -> f (0.,1)
;;