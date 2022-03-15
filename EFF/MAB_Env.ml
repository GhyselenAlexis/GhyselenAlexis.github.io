#use "EFF_List.ml" ;;
#use "MAB_Types.ml";;

(* A File for the Environment. Here, the user must specify how the algebraic operation interact with the environment*)

(* The "internal memory" of the environment (the comodel). Here, for multi-armed bandit, it only remembers which was the previous reward*)
type env_mem = float 

(* The reward of the machine a. It depends on a and it is obviously random. In a real case, this reward could be obtained by 
asking the user that really used the machine slot. *)
let reward (a:act) = (float_of_int a) +. (randomfloat 10.) ;;

(*One possible handler for the environment, the max corresponds to the number of machines *)
(* The machine m has a uniform reward between m and (m+3) *)
let mab_handler max = handler
  | y -> (fun (_:env_mem) -> y)
  | effect (Do a) k -> fun _ -> if (a > 0) && (a <= max) then begin 
      print ("You chosed Machine number " ^ (string_of_int a) ^ "\n"); (continue k ()) (reward a)
  end 
  else raise "This action is not available ! \n"
  | effect Observe k -> fun r -> (continue k r) r   
  | finally f -> f 0.
;;