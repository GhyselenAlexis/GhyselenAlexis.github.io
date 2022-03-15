(* print can print a string to the standard output *)
let print s = perform (Print s)

(* read can wait for an input from the user*)
let read () = perform (Read ())

(* raise can raise an exception, and print a specific string for the error*)
let raise s = absurd(perform (Raise s))

(*A somewhat equivalent to the "assume" function*)
let assume b s = if (not b) then raise s else ()

(* randomint n gives a random integer between 0 and n (0 included, n excluded)*)
let randomint n = perform (RandomInt n)

(* randomfloat x gives a random float between 0 and x *)
let randomfloat x = perform (RandomFloat x)

(* write f x evaluate f on x, and if f x = (xfile,s), then the string "s" is written on the file "xfile" *)
let write f x = perform (Write f x)
;;