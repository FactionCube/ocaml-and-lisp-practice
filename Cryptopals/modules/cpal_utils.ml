
module Cpal_utils =
  struct
        (* Convert a hexadecimal character to its ASCII hex value. *) 
        exception Not_a_hex_character
        exception Value_not_in_range_0_to_0xF

        (* Convert a single digit hex char to it's decimal value. *)
        let hexchar2int (c : char) =
        let open Stdlib in
          match c with
          | '0' .. '9' -> Char.code c - 48
          | 'a' .. 'f' -> Char.code c - 87
          | 'A' .. 'F' -> Char.code c - 55
          | _          -> raise Not_a_hex_character

        (* Convert the decimal value of a HEX character to an ASCII character. *)
        (* Same as function above, but using Core.    
            USE Char.of_int_exn instead. *)

        let hexdec2char (i : int) =
          match i with
          | i when i >= 0 && i <= 9 -> char_of_int (i+48)
          | i when i >= 10 && i <= 15 -> char_of_int (i+87)
          | _  -> Printf.printf "Supplied value: 0x%x\n" i ; raise Value_not_in_range_0_to_0xF 

        (* Generate a list of single-digit hex characters from a list of their decimal values. *)
        let i2hxch_lst (lst : int list) =
          let open Core in
          Iter.(of_list lst |> map (fun x -> hexdec2char x) |> to_list);;

        (* Generate an integer list from a list of hex chars. *)
        let hxch2i_lst (lst : char list) =
          let open Core in
          Iter.(of_list lst |> map (fun x -> hexchar2int x) |> to_list);;


        (* XOR two strings of hexadecimal characters. *)
        let computeXor (sa : string) (sb : string) =
        let open Core in
        let sal = hxch2i_lst (String.to_list sa) and sbl = hxch2i_lst (String.to_list sb) in
        let xor = List.map2_exn ~f:(fun a b -> a lxor b) sal sbl in
        i2hxch_lst xor |> String.of_char_list

        (* a b -> ab, logically. *)
        let cram (a : int) (b : int) = (a lsl 4) lxor b

        (* Generate a list of hex character pairs from a list of consecutive hex characters. *)
        let rec combine (lst : int list) =
        match lst with 
        | [] -> [] 
        | [x] ->  [x]
        | a::b::[] -> cram a b :: combine []
        | a::b::t  -> cram a b :: combine t

        let common_chars = Core.String.to_list "ETAOIN SHRDLU"

        (* Sum the occurrences of 'common_chars' within the decrypted plaintext.
          Use this to find the most likely  decrypt.  *)
        let rec often_char (cypher : string) (frequents : (char list)) (cnt : int) = 
        let open Core in
          match frequents with
          | [] -> cnt
          | h::t -> often_char cypher t (cnt + (String.count cypher ~f:(fun c -> phys_equal (Char.uppercase c) h) ))

        (* Record to store metadata and plaintext of the decipherment.  *)
        type cyxor = { freq: int ; key: char ; plain: string ; cipher: string }

        (* Get the list element for the highest frequency count.  I'm going to place 
           each cyxor record into a list. *)
        let rec get_maxF lst:(cyxor list) = 
        match lst with
        | [] -> []
        | a :: [] -> [a]
        | a :: b :: [] -> if a.freq >= b.freq then [a] else [b]
        | a :: b :: t -> if a.freq >= b.freq then get_maxF (a :: t) else get_maxF (b :: t)

        (* Sort a list according to increasing frequency count. *)
        let rec sort_cyxor (lst : cyxor list) =
          match lst with
            [] -> []
          | head :: tail -> insert head (sort_cyxor tail)
        and insert elt lst =
          match lst with
            [] -> [elt]
          | head :: tail -> if elt.freq <= head.freq then elt :: lst else head :: insert elt tail

        let hexbytes s = 
        let open Cryptokit in
          let hex = transform_string (Hexa.decode()) in 
          Bytes.of_string (hex s)

        (* Generate a range of plaintext results, each a result of solving
          the XOR for an instance from a range of monotonically increasing key bytes. *)
        let get_xor_char (rawhx : string) (first_key : int)  (last_key : int) : (cyxor list) =
        let open Cryptokit in
        let len = (String.length rawhx) / 2 in
        let lst = ref [] in
        let aux () =
        for i = first_key to last_key do
            (* let xor_key = (Char.of_int_exn i) in *)
            let xor_key = (char_of_int i) in
            let xorme = Bytes.make len xor_key in
            xor_bytes (hexbytes rawhx) 0 xorme 0 len;
            let plain = Bytes.to_string xorme in
            let ans = {freq = (often_char plain common_chars 0); key = xor_key; plain = plain; cipher = rawhx } in
            lst := (!lst @ [ans])
        done
          in aux ();
          List.rev !lst
  
        let print_cyxor (c:cyxor)  : unit =
          Printf.printf "\tFreq:- %i  Key:- %c \n\tPlain:- %s \n\tCipher:- %s \n" c.freq c.key c.plain c.cipher

        (* I'm presuming that the correct plaintext has the most occurrences of our common_chars *)
        let print_best_guess (cypher:string) =
          let best_guess =  
            get_maxF ( get_xor_char cypher 0x20 0x7e ) in 
              print_cyxor (Core.List.hd_exn best_guess)
  

  end




