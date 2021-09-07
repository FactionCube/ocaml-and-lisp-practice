(* Rewrite functions in cpal_utils.ml to work mainly with the List module. *)
(* This is to make data handling a little less cumbersome. *)
(* Use lists of ints.  That way, lxor is immediately accessible,
   and we can use handy List functions like map2, chunks_of etc. *)

module Cpal_simple =
  struct

    (* Use me in Bytes.map to XOR va with ch *)
    let char_of_xor (ch : char) (va : int) = 
        let open Stdlib in 
            char_of_int (Int.logxor (Char.code ch) va)

    let cipher_string (b64_infile : string) : 'a list =
        let open Cryptokit in
        let open Core in
        let open Stdint in
            In_channel.read_all b64_infile 
            (* Remove newlines *)
            |> String.filter ~f:(Char.( <> ) '\n')
            (* Get the underlying data *)
            |> transform_string ( Base64.decode() )
            |> String.to_list
            |> List.map ~f:(fun c -> int_of_char c ) 


    let do_pad (key : string) ?(modulus = 16) (chr : char) : (string) =
        let open Core in
            let pad_length = modulus - ((String.length key) % modulus) in
            Printf.printf "Null padding length: %d\n" pad_length;
            match pad_length with
            | 0 -> key
            | _ -> let tail_pad = Array.to_list (Array.create pad_length chr) in
                    String.of_char_list ( (String.to_list key) @ tail_pad ) 


    let computeXor (sa : int list) (sb : int list) : int list =
        let open Core in
        let open Stdint in
        List.map2_exn ~f:(fun a b -> a lxor b) sa sb  (* |> List.map ~f:(fun i -> Stdlib.Char.chr i)  *) 
        (* |> String.of_char_list  *)

    let decrypt (cypher:int list) (key:string) : string =
    let open Cryptokit in
    let open Core in
        let cypher_string = 
            List.map ~f:(fun i -> Stdlib.Char.chr i ) cypher |> Core.String.of_char_list in
        transform_string (Cipher.aes ~mode:Cipher.ECB key Cipher.Decrypt) cypher_string

    let encrypt (cypher:int list) (key:string) : string =
    let open Cryptokit in
    let open Core in
        let cypher_string = 
            List.map ~f:(fun i -> Stdlib.Char.chr i) cypher |> Core.String.of_char_list in
        transform_string (Cipher.aes ~mode:Cipher.ECB key Cipher.Encrypt) cypher_string

    let intlst_2_str (lst : int list) : string =
        List.map (fun i -> Char.chr i) lst |> Core.String.of_char_list

    let str_2_intlst (str : string) : int list =
    let open Core in
        String.to_list str |> List.map ~f:(fun c -> Stdlib.Char.code c)

    let rec string_list_to_int_list (lst : string list) : int list list =
        match lst with
        | [] -> []
        | [s] -> str_2_intlst s :: []
        | h :: t -> str_2_intlst h :: (string_list_to_int_list t)


    let rec cbc_enc (iv : int list ref)  (key:string) (cypher : int list list) : ('a list) =
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

    let rec cbc_dec (iv : int list ref)  (key:string) (cypher : int list list) : ('a list) =
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


    (* Generate a random key-string int list of arbitrary length *)
    let gen_rand_key_int_list (len : int) : int list =
        let open Core in
        let extra = ref [] in
        for i = 0 to (len - 1) do
            extra := (Random.int_incl 0 255) :: !extra
        done ;
        let outl = !extra in outl
        

    (* Generate an int list of random ints, ranging [0,255], with the list
    length varying randomly between 5 and 10. *)
    let add_random_list_head_tail (lst : int list) : int list =
        let open Core in
        let extra = ref [] in
        let len = Random.int_incl 5 10 in
        for i = 0 to len do
            extra := (Random.int_incl 0 255) :: !extra
        done ;
        let outl = !extra @ lst @ !extra in
        outl                    

  end

