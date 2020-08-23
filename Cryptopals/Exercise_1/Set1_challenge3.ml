(* Challenge 3 - Single Byte XOR.  
 * Scans a list of decrypted bytes, selecting the result which has 
 * the highest frequency count of common letters. 
 *)

open HexCraft
open Core
open Iter

let raw = "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736"
;;

(* XOR with a single character to decrypt the message. *)
let nxt = Iter.(of_str raw |> map (fun c -> hexchar2int c) |> to_list |> combine )
;;

(* val nxt : int list =
  [27; 55; 55; 51; 49; 54; 63; 120; 21; 27; 127; 43; 120; 52; 49; 51; 61; 120;
   57; 120; 40; 55; 45; 54; 60; 120; 55; 62; 120; 58; 57; 59; 55; 54]
*)

Iter.(of_list nxt |> map (fun c -> int2hexchar c ) |> to_str) ;;
(* - : string = "\02777316?x\021\027\127+x413=x9x(7-6<x7>x:9;76" *)

let alst = Iter.(of_list nxt |> map (fun c -> int2hexchar c ) |> to_list) ;;
(* val alst : char list =
  ['\027'; '7'; '7'; '3'; '1'; '6'; '?'; 'x'; '\021'; '\027'; '\127'; '+'; 'x';
   '4'; '1'; '3'; '='; 'x'; '9'; 'x'; '('; '7'; '-'; '6'; '<'; 'x'; '7'; '>';
   'x'; ':'; '9'; ';'; '7'; '6']
*)

let ans = Iter.(of_list nxt |> map (fun c -> c lxor 0x58) |> map (fun c -> int2hexchar c ) |> to_str) ;;
(* - : string = "Cooking MC's like a pound of bacon"  *)

let common_chars = Core.String.to_list "ETAOIN SHRDLU";;

let rec often_char input frequents cnt = 
match frequents with
| [] -> cnt
| h::t -> often_char input t (cnt + (String.count input ~f:(fun c -> phys_equal (Char.uppercase c) h) ))
;;

let ans_cnt = often_char ans common_chars 0
;;


(* Cryptokit solution *)
open Cryptokit
;;

let hex s = transform_string (Hexa.decode()) s
let hexbytes s = Bytes.of_string (hex s)
;;

(* Record to store metadata and plaintext of the decipherment.  *)
type cyxor ={ freq: int ; key: char ; plain: string }
;;

(* Get the list element for the highest frequency count. *)
let rec get_maxF lst = 
match lst with
| [] -> []
| a :: [] -> [a]
| a :: b :: [] -> if a.freq >= b.freq then [a] else [b]
| a :: b :: t -> if a.freq >= b.freq then get_maxF (a :: t) else get_maxF (b :: t)
;; 

(* Sort a list according to increasing frequency count. *)
let rec sort_cyxor lst =
   match lst with
     [] -> []
   | head :: tail -> insert head (sort_cyxor tail)
 and insert elt lst =
   match lst with
     [] -> [elt]
   | head :: tail -> if elt.freq <= head.freq then elt :: lst else head :: insert elt tail
 ;;

(* Generate a range of plaintext results, each a result of solving
   the XOR for an instance from a range of monotonically increasing key bytes. *)
let get_xor_char rawhx first_key last_key =
let len = (String.length rawhx) / 2 in
let lst = ref [] in
let aux () =
for i = first_key to last_key do
    let xor_key = (Char.of_int_exn i) in
    let xorme = Bytes.make len xor_key in
    xor_bytes (hexbytes rawhx) 0 xorme 0 len;
    let plain = Bytes.to_string xorme in
    let ans = {freq = (often_char plain common_chars 0); key = xor_key; plain = plain} in
    lst := (!lst @ [ans])
done
  in aux ();
  List.rev !lst
;;

(* I'm presuming that the correct plaintext has the most occurrences of our common_chars *)
let best_guess = get_xor_char raw 0x48 0x68 |> sort_cyxor |> get_maxF
;;

(* val best_guess : cyxor list =
  [{freq = 23; key = 'X'; plain = "Cooking MC's like a pound of bacon"}]   *)