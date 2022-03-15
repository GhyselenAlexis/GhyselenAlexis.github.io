(* In this file, we specify the types we need for our environment, here the multi armed bandit. 
The choice of those types depends on the environment, but the learner will only have a fixed abstraction of them *)

type env_obs = float 
type act = int

(* Usual effects in our theory *)
(* Observe can observe the environment *)
effect Observe : env_obs 
(* Do can modify the environment *)
effect Do : act -> unit 

(* Standard Notations*)
let observe () = perform Observe 
let do a = perform (Do a)
;; 