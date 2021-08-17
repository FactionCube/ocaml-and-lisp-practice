open Cpal_utils
open Cryptokit

let a = "1c0111001f010100061a024b53535009181c" and b = "686974207468652062756c6c277320657965"

let ans = computeXor a b
(* val ans : string = "746865206b696420646f6e277420706c6179"  *)

(* Alternative Cryptokit solution. *)
(* let hex s = transform_string (Hexa.decode()) s in
let hexbytes s = Bytes.of_string (hex s)
let tohex s = transform_string (Hexa.encode()) s
;;
*)
let hexbytes s = 
  let hex = transform_string (Hexa.decode()) in 
  Bytes.of_string (hex s)
;;

let tohex s = transform_string (Hexa.encode()) s
;;

let hex_a = hexbytes a and hex_b = hexbytes b in
  xor_bytes hex_a 0 hex_b 0 18; tohex (Bytes.to_string hex_b)
;;
