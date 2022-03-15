(* In this file, the user provides an abstraction of the environment for the learner. 
In practice, this file must implement all the types/function described in the interface for the learner.
Note that in EFF we cannot enforce something like that, but we should have in mind an Ocaml module 
when programming this file *)
#use "MAB_Env.ml" ;; 

(* Abstractions of observations. Note that a type unit means intuitively that the learner has no information. *)
 
type obs = unit  
;;

(* The effects that the learner can use: *)

(* Observe an abstraction of the usual observables *)
effect Abs_Observe : obs 

(* Get the list of all available actions for a given environment*)
effect Abs_GetAvailableAct : obs -> act list 

(* The set of all functions that we must provide to the learner*)
let abs_observe () = perform Abs_Observe
let abs_getavailableact (o : obs ) = perform (Abs_GetAvailableAct o)
;;


(*Transform a standard observation into an abstract one*)
let abstractobs (o : env_obs) :obs = () ;;

let abs_MAB max = let l = list_enumerate 1 (max + 1) in 
  handler 
  | effect Abs_Observe k ->  let o = observe () in continue k (abstractobs o) 
  | effect (Abs_GetAvailableAct o) k -> continue k l
;;

(* Effects the learner will have to implement*)

(* Choice gives an action to take, according to the internal policy*)
effect Choice : act  
(* Reward wait for a feedback, and then modify the internal memory and policy*)
effect Reward : float -> unit 

(* Standard Notations *)
let choice () = perform Choice 
let reward r = perform (Reward r)
;;