#use "../modules/bytes_cpal.ml"

open Cpal_simple
;;

let plain = "tinnies by the roadside are ugly, some may say, but late at night, they reflect the light, and safely guide the way!";;

let intlst_nulls = Core.Array.(create 128 '\000' 
                  |> map ~f:(fun a -> Stdlib.Char.code a) 
                  |> to_list) ;;

let plain = intlst_2_str intlst_nulls


(* Hic est encryption oracle, per exercitio 11. *)
(* The count of unique bytes, via Set module, correlates with the encryption mode
   inasmuch as a larger number indicates CBC.  This works well for plaintext which
   is a uniform stream of a single byte.  The test's reliability decreases as 
   plaintext variability increases. *)
let encryption_oracle (plain : string)  =
let open Cpal_simple in
let open Core in
    let key = intlst_2_str (gen_rand_key_int_list 16) and intlst = Cpal_simple.str_2_intlst plain in
    let random_added = add_random_list_head_tail intlst in
    let padded = str_2_intlst (do_pad (intlst_2_str random_added) '\000') in
    match Random.int 2 with
    | 0 -> (* ECB *) print_string "ECB: \n" ; 
                     str_2_intlst (encrypt padded key) 
                     |> Base.Set.of_list(module Core.Int) 
                     |> Core.Set.length 
    | 1 -> (* CBC *) print_string "CBC: \n" ; 
                     cbc_enc (ref [0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0]) key (List.chunks_of ~length:16 padded)
                     |> Stdlib.String.concat "" 
                     |> str_2_intlst 
                     |> Base.Set.of_list(module Core.Int) 
                     |> Core.Set.length 
    | _ -> failwith "Impossible 3rd number from binary-choice (Random.int 2) "
(*

val plain : string =
  "\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"

utop # encryption_oracle plain;;
Null padding length: 14
CBC: 
- : int = 123

utop # encryption_oracle plain;;
Null padding length: 12
CBC: 
- : int = 115

utop # encryption_oracle plain;;
Null padding length: 16
CBC: 
- : int = 122

utop # encryption_oracle plain;;
Null padding length: 12
CBC: 
- : int = 127

utop # encryption_oracle plain;;
Null padding length: 2
ECB: 
- : int = 42

utop # encryption_oracle plain;;
Null padding length: 16
ECB: 
- : int = 46

utop # encryption_oracle plain;;
Null padding length: 12
ECB: 
- : int = 60

utop # encryption_oracle plain;;
Null padding length: 16
CBC: 
 - : int = 118 
 *)


(* The oracle is completely unreliable once you add any plain text of substance.
utop # let plain = "tinnies by the roadside are ugly, some may say, but late at night, they reflect the light, and safely guide the way!";;
val plain : string =
  "tinnies by the roadside are ugly, some may say, but late at night, they reflect the light, and safely guide the way!"

utop # encryption_oracle plain;;
Null padding length: 10
ECB: 
- : int = 104

utop # encryption_oracle plain;;
Null padding length: 6
CBC: 
- : int = 116

utop # encryption_oracle plain;;
Null padding length: 14
ECB: 
- : int = 115

utop # encryption_oracle plain;;
Null padding length: 12
CBC: 
- : int = 110

utop # encryption_oracle plain;;
Null padding length: 8
CBC: 
- : int = 102
*)