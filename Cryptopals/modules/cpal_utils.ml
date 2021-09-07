
module Cpal_utils =
  struct
        (* Convert a hexadecimal character to its ASCII hex value. *) 
        exception Not_a_hex_character of (unit)
        exception Value_not_in_range_0_to_0xF

        (* Take a list a convert it to a list of pairs. *)
        let rec pairs (l : 'a list) =
        match l with
          | [] -> []
          | [_] -> []
          | a::b::[] -> (a,b) :: pairs []
          | a::b::tl -> (a,b) :: pairs tl

        (* Convert a single digit hex char to it's decimal value. *)
        let hexchar2int (c : char) = 
        let open Stdlib in
          match c with
          | '0' .. '9' -> Char.code c - 48
          | 'a' .. 'f' -> Char.code c - 87
          | 'A' .. 'F' -> Char.code c - 55
          | _          -> raise (Not_a_hex_character (Printf.printf "Bad character is:  %c\n" c )) 

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

        let hexbytes (s:string) = 
        let open Cryptokit in
          let hex = transform_string (Hexa.decode()) in 
          Bytes.of_string (hex s)

        let hex = 
          let open Cryptokit in
            transform_string (Hexa.decode());;

        let tohex (s:string) = 
          let open Cryptokit in
            transform_string (Hexa.encode()) s

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

        let print_cyxor_key (c:cyxor) : unit =
          print_char c.key

        (* I'm presuming that the correct plaintext has the most occurrences of our common_chars *)
        let print_best_guess (cypher:string) =
          let best_guess =  
            get_maxF ( get_xor_char cypher 0x20 0x7e ) in 
              print_cyxor (Core.List.hd_exn best_guess)

        let mix plain cipher = 
        let open Stdlib in
          let len = Bytes.length cipher in
          let mixed = Bytes.mapi (fun i c -> 
            char_of_int (( int_of_char c) lxor ( int_of_char (Bytes.get cipher (i mod len) ))) ) plain
            in mixed

        (* Hamming number is the count of active bits in a binary number. *)
        let rec hamming (byt:int) =
        match byt with 
        | 0 -> 0
        | b -> (b land 1) + hamming (b lsr 1)

        (* Take two bytes, XOR them, and calculate the hamming number. *)
        let ham_of_xored_bytes (a: char) (b: char) =
          let open Stdint in
          let aa = Uint8.of_int (int_of_char a) and bb = Uint8.of_int (int_of_char b) in 
              let ham = Uint8.logxor aa bb |> Uint8.to_int |> hamming
          in ham

        (* Compute the normalised Hamming Distances for slices across the ciphertext. 
          If each slice is a segment in a chain, then adding more segments allows 
          the correct keysize to rise to the lowest hamming distance value.  
        *)
        let normal_hamming (cipherstr:string) =
        let ham_lst = ref [] in
        let open Stdlib in
            for keysz = 1 to 40 do
                let sum = ref 0.0 in
                for slice = 0 to 39 do         (* Select 40 intervals across the cipher *)
                (* let keysz = 5 in  *)
                let fst = String.sub cipherstr (0 + slice) keysz and snd = Stdlib.String.sub cipherstr (keysz + slice) keysz
                in
                    let flst = Core.String.to_list fst and slst = Core.String.to_list snd in
                    let start = ham_of_xored_bytes '\000' '\000' in 
                    let ham = Core.List.fold2_exn flst slst ~init:start
                        ~f:(fun s l1 l2 -> s + (ham_of_xored_bytes l1 l2  ) ) in
                    let norm = !sum +. ( float_of_int ham /. float_of_int keysz ) in 
                    sum := norm
                done ;
                sum := !sum /. 40.0;
                ham_lst := (keysz, ( !sum /. 4.0) ) :: !ham_lst ;
                Printf.printf " %i   %3.2f " keysz !sum; 
            done;
            !ham_lst

        let rec insert (x : (int * float) ) (l : (int * float) list) =
            match l,x with 
            | [],x -> [x]
            | ((k1,v1)::t) , (ky,vl) ->  
                if Float.compare v1 vl >= 0 
                    then x :: (k1,v1) :: t
                    else (k1,v1) :: insert x t 


        let rec sort (l : (int * float) list) =
            match l with 
            | [] -> []
            | h::t -> insert h (sort t)

        let print_pair (x: (int * float) option) : unit = 
            match x with 
            | Some (k,v) -> Printf.printf "\n\nSuggested key length: %i  |hamming|: %f\n" k v
            | None -> print_string "Empty keyval\n"

        (* Use this to extract characters which are a fixed distance apart
          that distance being the putative keylength, such that the chars
          collected will be amenable to XOR with a range of key bytes,
          such that the frequency count of common letters for each key byte
          can be collected and ranked.   In other words, the byte stream
          so gathered will be decrypted, via single substitution, to the
          most likely 'plaintext'.  *)
        let extract (start:int) (stride:int) (lst: 'a list) =
        let open Core in
            let len = List.length lst - start in
            let smaller = List.sub lst ~pos:start ~len:len in
            List.filteri smaller ~f:( fun i _ -> (i % stride) = 0 )

        let check_keys (keylength:int) (cypher:string) =
        let open Core in
            let aux () =
            for i = 0 to (keylength - 1) do
                let str = String.to_list cypher |> extract i keylength |> String.of_char_list |> tohex in
                let guess0 = get_xor_char str 0x20 0x7e |> get_maxF in 
                    print_cyxor (List.hd_exn guess0)
                done in aux ()

        let mix (plain:bytes) (cipher:bytes) = 
        let open Stdlib in
          let len = Bytes.length cipher in
          let mixed = Bytes.mapi (fun i c -> 
            Char.chr (( Char.code c) lxor ( Char.code (Bytes.get cipher (i mod len) ))) ) plain
            in mixed

        let decrypt (key:string) (cipher:string) =
          let open Core in
          let open Cryptokit in
          let buf = Bytes.of_string cipher and cip = Bytes.of_string key in 
          let ans = transform_string (Hexa.encode()) (Bytes.to_string (mix buf cip)) |> hex in ans

        let dump_likely_keysize (cipher:string) : ((int * float) option) =
        let open Core in
        let get_ham_list = normal_hamming cipher in
        let sorted = sort get_ham_list in
            let keyval = List.hd sorted in 
                print_pair keyval; keyval

        let dump_key (cipher:string) : unit =
        let open Core in
            for stride = 0 to 28 do
                let column = String.to_list cipher 
                            |> extract stride 29 
                            |> String.of_char_list 
                            |> tohex in      
                let keychar = get_xor_char column  0x20 0x7e |> get_maxF in
                print_cyxor_key (List.hd_exn keychar)
            done
            ; print_string "\n"

  end




