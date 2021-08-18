open Stdint
open Core
open Bigarray
open Cryptokit
open Cpal_utils

(*  #require core,stdint,bigarray,cryptokit ;; *)

(* The given start example in Challenge 1_6   Should give an answer of 37. *)
let _ =
    let open Core in
    let wok = String.to_list "wokka wokka!!!" and this = String.to_list "this is a test" in
    let start = ham_of_xored_bytes '\000' '\000' in 
    let res = List.fold2_exn wok this ~init:start
        ~f:(fun s l1 l2 -> s + (ham_of_xored_bytes l1 l2  ) ) 
        in res

(* - : int = 37 *)


let cipher_string = 
    let open Cryptokit in 
    let open Core in
        In_channel.read_all "./6.txt"
        (* Remove newlines *)
        |> String.filter ~f:(Char.( <> ) '\n')
        (* Get the underlying data *)
        |> transform_string ( Base64.decode() )
;;

dump_likely_keysize cipher_string
;;


(*
let str_cip0 = String.to_list cipher_string |> extract 0 29 |> String.of_char_list |> tohex;;

let () = 
    let open Core in
        let guess0 = get_xor_char str_cip0  0x20 0x7e |> get_maxF in 
        print_cyxor (List.hd_exn guess0) ; print_newline ()
*)

dump_key cipher_string
;;

(* Terminator X: Bring the noise
   - : unit = ()  *)

let result = 
    let plain = decrypt "Terminator X: Bring the noise" cipher_string in
    print_string plain
;;


