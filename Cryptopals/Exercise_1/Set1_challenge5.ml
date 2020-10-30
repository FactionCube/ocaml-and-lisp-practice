(* require "cryptokit" in utop *)
open Cryptokit

let str =  "Burning 'em, if you ain't quick and nimble\nI go crazy when I hear a cymbal";;

let buf = Bytes.of_string str;;
let xor = "ICE";;
let cip = Bytes.of_string xor;;

(* I've discovered the usefulness of Bytes.mapi. *)

let mix plain cipher = 
  let len = Bytes.length cipher in
  let mixed = Bytes.mapi (fun i c -> 
    Char.chr (( Char.code c) lxor ( Char.code (Bytes.get cipher (i mod len) ))) ) plain
    in mixed

let ans = transform_string (Hexa.encode()) (Bytes.to_string (mix buf cip))

;;
