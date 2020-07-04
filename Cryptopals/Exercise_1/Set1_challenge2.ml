(*
#require "core";;
#require "iter";;
*)

open HexCraft

let a = "1c0111001f010100061a024b53535009181c" and b = "686974207468652062756c6c277320657965"
;;


let computeXor sa sb =
  let open Core in
  let sal = hxch2i (String.to_list sa) and sbl = hxch2i (String.to_list sb) in 
  let xor = List.map2_exn ~f:(fun a b -> a lxor b) sal sbl in
  i2hxch xor |>String.of_char_list
  in computeXor a b

(* val ans : string = "746865206b696420646f6e277420706c6179"  *)
;;

