(* Comment/Uncomment to chose the good environment for compiling the file alone*)

(* #use "MAB_Abstract.ml" ;; *)
(* #use "MABStates_Abstract.ml" ;; *)

 (*Naive algorithm with step-size parameter for nonstationnary rewards*)

(* An additional effect for debugging*)
effect Printestimation : unit
let printestimation () = perform Printestimation 
;; 

(*The internal memory (the comodel) of the learner that remembers estimation for immediate rewards. It consists in: 
  -a list of estimation for each couple (obs,act) 
  -the index of the previous choice, to remember to which choice a reward correspond*)
type memory = (obs * ((act * float) list)) list * int * int

(* Gives the next estimation of the average reward with a reward of zero *)
let nextestimate alpha (a,v) = (a,v -. (alpha *. v))

(* Update the list of estimation l after a choice with index na*)
let updatechoice l na alpha = list_setnth l na (nextestimate alpha)

(* Modify an estimation according to a reward*)
let nextreward r alpha (a,v)  = (a,v +. (alpha *. r))

(* Update the list of estimation l after a reward r for the choice with index na*)
let updatereward l na r alpha = list_setnth l na (nextreward r alpha) 

(*Find in the list of estimation the choice with index na*)
let findact l na = let (a,_) = list_nth l na in a 

(*Find the choice that maximizes the estimated reward in the list l*)
let argmax l = let (na,(a,_)) = (list_max l (fun (_,v) (_,u) -> v >= u)) in (na,a)

(* Transform an estimation into a string for printing*)
let string_of_pair (a,v) = "(" ^ (string_of_float v) ^ ")" 

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
      let lact = map (fun a -> (a,v)) (abs_getavailableact o) in ((o,lact)::l,0,lact) 
    end
;;

(* Update the list of estimation lest for the state with index n, using the function f *)
let updatestate l no f = (list_setnth l no (fun (o,q) -> (o,f q))) ;;

(*An IA with a bit more complexity : for each observable state, it remembers the average immediate reward for each action
Then, when the IA has to make a choice, it does a p-greedy policy : 
  -with probability p it choses an action at random
  -with probability (1-p) it choses the action with the best estimated reward  *)
let ia_pgreedy_estimate p v alpha   = handler (*p is the probability of exploring, v the initial estimation, alpha the step-size parameter*)
| y -> (fun (_:memory) -> y)
| effect Choice k -> fun (l,_,_) -> 
  let o = abs_observe () in 
  let (l',no,q) = getstateestimate l o v in
  let (na,a) = pgreedypolicy (p,q) in
  let l'' = updatestate l' no (fun ll -> updatechoice ll na alpha) in 
  (continue k a) (l'',no,na) 
| effect (Reward r) k -> fun (l,no,na) -> 
  let l' = updatestate l no (fun q -> updatereward q na r alpha) in 
  (continue k ()) (l',no,na) 
| effect Printestimation k -> fun (l,no,na) -> let _ = map (fun (o,q) -> list_print (map string_of_pair q)) l in (continue k ()) (l,no,na)
| finally f -> f ([],0,0) 
;;    

