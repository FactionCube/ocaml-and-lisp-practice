(* Challenge 3 - Single Byte XOR.  
 * I should update this to scan a list of cipher bytes,
 * selecting the correct one using frequency counts. 
 *)

open HexCraft

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

Iter.(of_list nxt |> map (fun c -> c lxor 0x58) |> map (fun c -> int2hexchar c ) |> to_str) ;;
(* - : string = "Cooking MC's like a pound of bacon"  *)

;;


