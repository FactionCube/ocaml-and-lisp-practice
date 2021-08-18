(* require "cryptokit" in utop *)
open Cryptokit

let str =  "Burning 'em, if you ain't quick and nimble\nI go crazy when I hear a cymbal";;

let buf = Bytes.of_string str;;
let xor = "ICE";;
let cip = Bytes.of_string xor;;

(* I've discovered the usefulness of Bytes.mapi. *)

(* alternative
let mix plain cipher = 
let open Stdlib in
  let len = Bytes.length cipher in
  let mixed = Bytes.mapi (fun i c -> 
    Char.chr (( Char.code c) lxor ( Char.code (Bytes.get cipher (i mod len) ))) ) plain
    in mixed
*)

let mix plain cipher = 
let open Stdlib in
  let len = Bytes.length cipher in
  let mixed = Bytes.mapi (fun i c -> 
    char_of_int (( int_of_char c) lxor ( int_of_char (Bytes.get cipher (i mod len) ))) ) plain
    in mixed


let ans = transform_string (Hexa.encode()) (Bytes.to_string (mix buf cip))
(* val ans : string =
  "0b3637272a2b2e63622c2e69692a23693a2a3c6324202d623d63343c2a26226324272765272
   a282b2f20430a652e2c652a3124333a653e2b2027630c692b20283165286326302e27282f" *)
;;
