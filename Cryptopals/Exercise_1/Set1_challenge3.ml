(* Challenge 3 - Single Byte XOR.  
 * Scans a list of decrypted bytes, selecting the result which has 
 * the highest frequency count of common letters. 
 *)

(* open HexCraft  *)
open Cpal_utils
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

Iter.(of_list nxt |> map (fun c -> char_of_int c ) |> to_str) ;;
(* - : string = "\02777316?x\021\027\127+x413=x9x(7-6<x7>x:9;76" *)

let alst = Iter.(of_list nxt |> map (fun c -> char_of_int c ) |> to_list) ;;
(* val alst : char list =
  ['\027'; '7'; '7'; '3'; '1'; '6'; '?'; 'x'; '\021'; '\027'; '\127'; '+'; 'x';
   '4'; '1'; '3'; '='; 'x'; '9'; 'x'; '('; '7'; '-'; '6'; '<'; 'x'; '7'; '>';
   'x'; ':'; '9'; ';'; '7'; '6']
*)

let ans = Iter.(of_list nxt |> map (fun c -> c lxor 0x58) |> map (fun c -> char_of_int c ) |> to_str) ;;
(* - : string = "Cooking MC's like a pound of bacon"  *)

let ans_cnt = often_char ans common_chars 0
;;

print_best_guess raw
;;

(* val best_guess : cyxor list =
  [{freq = 23; key = 'X'; plain = "Cooking MC's like a pound of bacon"}]   

val print_cyxor : cyxor -> unit = <fun>
	Freq:- 23  Key:- X 
	Plain:- Cooking MC's like a pound of bacon 
	Cipher:- 1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736 
- : unit = ()
  
  *)


(* Here's Challenge 4 from Exercise 1. *)

let cipherlist = In_channel.read_lines "./4.txt" in
let best = ref [] in
let aux () =
  List.iter ~f:(fun a -> best := !best @ (get_maxF (get_xor_char a 0x20 0x7e))) cipherlist
  in aux ();
  print_cyxor (List.hd_exn (get_maxF !best))
  (* get_maxF !best *)
;;

(*	Freq:- 22  Key:- 5 
	Plain:- Now that the party is jumping
 
	Cipher:- 7b5a4215415d544115415d5015455447414c155c46155f4058455c5b523f 
*)
