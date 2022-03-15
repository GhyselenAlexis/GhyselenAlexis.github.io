(* Comment/Uncomment to chose the good environment for compiling the file alone*)

(* #use "MAB_Abstract.ml" ;; *)
(* #use "MABStates_Abstract.ml" ;; *)

(* A File for the Naive algorithm. *)

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

(*Combine observe and getavailableact*)
let obsandget () = let o = abs_observe () in (o,abs_getavailableact o)

(* A very naive IA for tests, it always choses the first available action *)
let ia_constant = handler 
  | y -> (fun _ -> y)
  | effect Choice k -> fun m -> let (_,l) = obsandget () in (continue k (hd l)) m
  | effect (Reward r) k -> continue k ()  
  | finally f -> f () 
;;

(*The internal memory (the comodel) of the pgreedy learner that remembers only immediate expected reward. It consists in: 
  -a list of estimation for each couple (obs,act), with the number of time this choice has been made 
  -the index of the previous choice, to remember to which choice a reward correspond*)
type memory = (obs * ((act * float * float) list)) list * int * int

(* Gives the next estimation of the average reward with a reward of zero *)
let nextestimate (a,v,t) = (a,v *. (t +. 1.) /. (t +. 2.) ,t +. 1.)

(* Update the list of estimation l after a choice with index na*)
let updatechoice l na = list_setnth l na nextestimate

(* Modify an estimation according to a reward*)
let nextreward r (a,v,t) = (a,v +. r /. (t +. 1.),t)

(* Update the list of estimation l after a reward r for the choice with index na*)
let updatereward l na r = list_setnth l na (nextreward r) 

(*Find in the list of estimation the choice with index na*)
let findact l na = let (a,_,_) = list_nth l na in a 

(*Find the choice that maximizes the estimated reward in the list l*)
let argmax l = let (na,(a,_,_)) = (list_max l (fun (_,v,_) (_,u,_) -> v >= u)) in (na,a)

(* Transform an estimation into a string for printing*)
let string_of_pair (a,v,t) = "(" ^ (string_of_float v) ^ "," ^ (string_of_float t) ^ ")" 

(* The policy is to select a random action with some probability p and otherwise to take the better option according to the estimation. 
The input is the current list of estimation for the observed state.
This function returns an action with its index*)
let pgreedypolicy (p,l) = 
  if ((randomfloat 1.) <= p) then 
    begin 
    print "Let's Explore ! \n";
    let len = length l in  
    let na = randomint len in 
    let a = findact l na in 
    (na,a)
    end  
  else 
    argmax l
;;

(* Find the index and the estimation list corresponding to the observed state o in l *)   
let rec findobs l o = match l with 
    | [] -> (false,0,[])
    | (x,y)::q -> if (o = x) then (true,0,y) else let (b,no,s) = (findobs q o) in (b,no + 1,s)  
;;

(* Find the index no' and the list of estimation q' of the state o in the list l. 
If the list l does not contain estimation for the state o, we initialize a new list of estimation with initial value v *)
let getstateestimate l o v = 
  let (b,no,q) = findobs l o in 
  if b then (l,no,q) else 
    begin
      let lact = map (fun a -> (a,v,0.)) (abs_getavailableact o) in ((o,lact)::l,0,lact) 
    end
;;

(* Update the list of estimation lest for the state with index n, using the function f *)
let updatestate l no f = (list_setnth l no (fun (o,q) -> (o,f q))) ;;

(*An IA with a bit more complexity : for each observable state, it remembers the average immediate reward for each action
Then, when the IA has to make a choice, it does a p-greedy policy : 
  -with probability p it choses an action at random
  -with probability (1-p) it choses the action with the best estimated reward  *)
(* The second parameter gives the initial estimation of the IA*)
let ia_pgreedy_estimate p v = handler (*p is the probability of exploring, v the initial estimation*)
| y -> (fun (_:memory) -> y) (*declare a state monad, with the type described above*)
| effect Choice k -> fun (l,_,_) -> 
  let o = abs_observe () in (*use the interface to get an observation*)
  let (l',no,q) = getstateestimate l o v in (* extract the index no corresponding to o, with its list of estimation *)
  let (na,a) = pgreedypolicy (p,q) in (* select the action a with the p-greedy policy *)
  let l'' = updatestate l' no (fun ll -> updatechoice ll na) in (*update the estimations*)
  (continue k a) (l'',no,na) (*return action a, with the updated memory*)
| effect (Reward r) k -> fun (l,no,na) -> 
  let l' = updatestate l no (fun q -> updatereward q na r) in (*update estimations for the previous choice (no,na) *)
  (continue k ()) (l',no,na) (*give the new memory to the continuation*)
| effect Printestimation k -> fun (l,no,na) -> let _ = map (fun (o,q) -> list_print (map string_of_pair q)) l in (continue k ()) (l,no,na)
| finally f -> f ([],0,0) (*initial memory*)
;;    

