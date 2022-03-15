#use "EFF_Primitives.ml" ;;

(* In this file, we give some basic functions for lists. In order to have some uniformity, all function nanes start with "list_"*)

(*Create the list of integers from m to n (m included, n excluded) *)
let rec list_enumerate m n = 
  if m >= n then [] else m::(list_enumerate (m+1) n)
;;

(* Return the maximum element of the list, for the "greater" function, and give its index with the shape: (index,value)*)
let rec list_max l greater = match l with 
  | [] -> raise "Maximum of an Empty List \n"
  | [a] -> (0,a) 
  | x::q -> let (n,y) = list_max q greater in if (greater x y) then (0,x) else (n+1,y)
;;

(* Return the nth element of a list *)
let rec list_nth l n = match l with 
  | [] -> raise ("This list has no " ^ (string_of_int n) ^ "th element \n") 
  | x::q -> if n = 0 then x else (list_nth q (n-1))
;;  

(* Apply f to the nth element of a list*)
let rec list_setnth l n f = match l with 
  | [] -> print ("Warning : No " ^ (string_of_int n) ^ "th element to modify \n"); [] 
  | x::q -> if n = 0 then (f x)::q else x::(list_setnth q (n-1) f)
;;  

(*Transform a list of string into a string*)
let string_of_stringlist l = 
  let rec printaux q = match q with 
      | [] -> "] \n" 
      | [x] -> x ^ "] \n" 
      | x::r -> x ^ ";" ^ (printaux r)
    in ("[" ^ (printaux l)) 
  ;;

(* Print a list of string *)
let list_print l = print (string_of_stringlist l);;
