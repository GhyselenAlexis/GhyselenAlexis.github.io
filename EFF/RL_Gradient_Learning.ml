(* Comment/Uncomment to chose the good environment for compiling the file alone*)

(* #use "MAB_Abstract.ml" ;; *)
(* #use "MABStates_Abstract.ml" ;; *)

(* A File for the Gradient Learning algorithm. *)

(* Recall : We consider learner with access to this interface : 
type act
type obs 

abs_observe : unit -> obs 
abs_getavailableact : obs -> act list 
*)

(* An additional effect for debugging*)
effect Printestimation : unit
let printestimation () = perform Printestimation 
;; 

(*The internal memory (the comodel) of the gradient learning learner. It consists in: 
  -For each state (obs), the following information for preferences:
    * a list of preferences associated to each action 
    * the expected immediate reward for this state
    * the number of time a choice has been made in this state 
  -the index of the previous choice, to remember to which choice a reward correspond*)
type memory = ((obs * (((act * float) list) * float * float)) list) * int * int 

(*Tranform a list of (action,preferences) to a list of probability for each action*)
let pref_to_proba l = 
  let rec pref_exponent l = match l with
    | [] -> ([],0.)
    | (a,h)::q -> 
      let (l',s') = pref_exponent q in
      let eh = exp h in 
      ((a,eh)::l',eh +. s')
  in let (l',s) = pref_exponent l in map (fun (a,x) -> (a, x /. s)) l'
;;

(*Take a list of couples (action,probability) and a random number in [0,1], 
and chose one action according to the probabilities, and give its index *)
let rec choose_action l x = match l with 
  | [] -> raise "Error: you tried to choose an action in an empty list"
  | (a,p)::q -> if x <= p then (0,a)
    else let (na,a) = choose_action q (x -. p) in (na + 1,a)
;; 

(* The policy takes a list of couples (act,preferences) and choose one action in it *)
let gradient_policy l = choose_action (pref_to_proba l) (randomfloat 1.) ;;

(*Given an expected reward, a reward, a step size-parameter, the index of the previous choice, the list of probability
and the list of preferences, returns the updated list*)
let rec update_preferences rexp r alpha na pl hl = match (pl,hl) with 
  | ([],[]) -> []
  | ([],_) -> raise "Error: The computation of the probability list must have failed"
  | (_,[]) -> raise "Error: The computation of the preferences list must have failed"
  | ((a,p)::pq,(b,h)::hq) -> assume (a = b) "Error : Mismatch of actions";
  let hfin = update_preferences rexp r alpha (na - 1) pq hq in 
  if na = 0 then ((a,(h +. alpha *. (r -. rexp) *. (1. -. p)))::hfin)
  else ((a,(h -. alpha *. (r -. rexp) *. p))::hfin)
;;

(* In a chosen state, given the reward, a step-size parameter, the index of the previous choice and a memory, 
returns the updated memory *) 
let update_memory r alpha na (hl,rexp,t) = 
  let postrexp = (rexp *. t +. r) /. (t +. 1.) in 
  let pl = pref_to_proba hl in 
  (update_preferences postrexp r alpha na pl hl,postrexp,t +. 1.)

(* Use the previous function for the state with index no given a list for all states l *)
let updatestate r alpha na no l = (list_setnth l no (fun (o,m) -> (o,update_memory r alpha na m))) ;;  

(* Find the index and the memory corresponding to the observed state o in l *)   
let rec findobs l o = match l with 
| [] -> (false,0,([],0.,0.))
| (x,m)::q -> if (o = x) then (true,0,m) else let (b,no,mm) = (findobs q o) in (b,no + 1,mm)  
;;

(* Find the index and the memory of the state o in the list l. 
If the list l does not contain estimation for the state o, we initialize a new list of estimation with initial preference v *)
let getstatememory l o v = 
let (b,no,m) = findobs l o in 
if b then (l,no,m) else 
begin
  let lact = map (fun a -> (a,v)) (abs_getavailableact o) in 
  let mm = (lact,0.,0.) in 
  ((o,mm)::l,0,mm) 
end
;;

(* Transform a pair into a string for printing*)
let string_of_pair (a,v) = "(" ^ (string_of_int a) ^ "," ^ (string_of_float v) ^ ")" 

(* Tranform a memory for a fixed state into a string for printing*)
let string_of_fixedmemory (hl,r,t) = "\n " 
  ^ "Preferences:" ^ (string_of_stringlist (map string_of_pair hl)) ^ "\n"
  ^ "Expect: (" ^ (string_of_float r) ^ "," ^ (string_of_float t) ^ ") \n" 
;;

(*The gradient learning IA where:
  -The first parameter is the step-size parameter
  -The second parameter is the initial preferences *)
let ia_gradient alpha v = handler 
| y -> (fun (_:memory) -> y)
| effect Choice k -> fun (l,_,_) -> 
  let o = abs_observe () in 
  let (l',no,(hl,_,_)) = getstatememory l o v in
  let (na,a) = gradient_policy hl in
  (continue k a) (l',no,na)
| effect (Reward r) k -> fun (l,no,na) -> 
  let l' = updatestate r alpha na no l in
  (continue k ()) (l',no,na) 
| effect Printestimation k -> fun (l,no,na) -> let _ = map (fun (o,m) -> print (string_of_fixedmemory m)) l in (continue k ()) (l,no,na)
| finally f -> f ([],0,0)
;;    


