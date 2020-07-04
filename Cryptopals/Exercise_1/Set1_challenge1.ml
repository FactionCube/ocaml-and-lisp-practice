(*
#require "hex";;
#require "base64";;



#show Hex;;
module Hex :
sig
type t = [ `Hex of string ]
val of_char : char -> char * char
val to_char : char -> char -> char
val of_string : ?ignore:char list -> string -> t
val to_string : t -> string
val of_bytes : ?ignore:char list -> bytes -> t
val to_bytes : t -> bytes
val of_cstruct : ?ignore:char list -> Cstruct.t -> t
val to_cstruct : t -> Cstruct.t
val of_bigstring : ?ignore:char list -> Cstruct.buffer -> t
val to_bigstring : t -> Cstruct.buffer
val hexdump : ?print_row_numbers:bool -> ?print_chars:bool -> t -> unit
val hexdump_s : ?print_row_numbers:bool -> ?print_chars:bool -> t -> string
val pp : Format.formatter -> t -> unit
val show : t -> string
end
*)

let str = "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d";;
(* val str : string =
            "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d"  *)

let hex_t = `Hex str;;
(* val hex_t : [> `Hex of string ] =
              `Hex
                  "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d" *)

let binary = Hex.to_string hex_t;;
(* val binary : string = "I'm killing your brain like a poisonous mushroom"  *)

let ans = Base64.encode binary;;
(* val ans : (string, [> `Msg of string ]) result =
            Ok "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t"  *)


