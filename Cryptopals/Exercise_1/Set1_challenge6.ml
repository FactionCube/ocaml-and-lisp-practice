open Stdint
open Core

  let a = Stdlib.Char.code 'a'
  let b = Stdlib.Char.code  'b'
(* val a : int = 97
    val b : int = 98  
*)
;;

(* Hamming number is the count of active bits in a binary number. *)
let rec hamming byt =
match byt with 
| 0 -> 0
| b -> (b land 1) + hamming (b lsr 1)

(* Take two bytes, XOR them, and calculate the hamming number. *)
let ham_of_xored_bytes a b =
let aa = Uint8.of_int (Stdlib.Char.code a) and bb = Uint8.of_int (Stdlib.Char.code b) in 
    let ham = Uint8.logxor aa bb |> Uint8.to_int |> hamming
in ham
;;

(* The given start example in Challenge 1_6 *)
let wok = "wokka wokka!!!" and this = "this is a test" in
let woklst = String.to_list wok and thislst = String.to_list this in
let start = ham_of_xored_bytes '\000' '\000' in 
let res = List.fold2_exn woklst thislst ~init:start
    ~f:(fun s l1 l2 -> s + (ham_of_xored_bytes l1 l2  ) ) 
    in res
(* - : int = 37 *)

;;

