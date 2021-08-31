#use "../modules/bytes_cpal.ml"
;;

let key = "YELLOW SUBMARINE"
;;

let iv = Cpal_simple.do_pad "YELLOW SUBMARINE" '\000'
;;

let cypher = 
    let open Cpal_simple in 
        cipher_string "10.txt"
;;

(* Given an int list, partition the list into a list of N-long int lists *)
let chunky = Core.List.chunks_of ~length:16 cypher
;;


let rec cbc_enc (iv : int list ref)  (key:string) (cypher : int list list) : ('a list) =
let open Cpal_simple in
let open Core in
let enc_str = ref "" in
match cypher with
| [] -> []
| [a] ->        iv := (computeXor !iv a); 
                enc_str := encrypt !iv key;
                iv := str_2_intlst !enc_str;
                !enc_str :: (cbc_enc iv key [])
| a :: t ->     iv := computeXor !iv a; 
                enc_str := encrypt !iv key;
                iv := str_2_intlst !enc_str;
                !enc_str :: (cbc_enc iv key t)
;;

let rec cbc_dec (iv : int list ref)  (key:string) (cypher : int list list) : ('a list) =
let open Cpal_simple in
let open Core in
let dec_str = ref "" in
match cypher with
| [] -> []
| [a] -> dec_str := decrypt a key;
            let tmp = computeXor (str_2_intlst !dec_str) !iv in   
                iv := a;
                (intlst_2_str tmp) :: (cbc_dec iv key [])
| a :: t -> dec_str := decrypt a key;
            let tmp = computeXor (str_2_intlst !dec_str) !iv in   
                iv := a;
                (intlst_2_str tmp) :: (cbc_dec iv key t )
;;


let myiv = ref [0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0];;

let solution = cbc_dec myiv key chunky;;

let solution_string = String.concat "" solution;;

print_string solution_string;;

let inclean = Cpal_simple.string_list_to_int_list ["I'm back and I'm"; " ringin' the bel"];;

let encrypted = cbc_enc (ref [0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0]) key inclean;;

let out_enc = Cpal_simple.string_list_to_int_list encrypted;;

let decrypted = cbc_dec (ref [0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0]) key out_enc;;
