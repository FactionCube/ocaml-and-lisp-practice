(* This is a work in progress. It contains a lot of draft code. *)

#use "../modules/bytes_cpal.ml"

open Cpal_simple
;;

let text_b64 = "Um9sbGluJyBpbiBteSA1LjAKV2l0aCBteSByYWctdG9wIGRvd24gc28gbXkg
aGFpciBjYW4gYmxvdwpUaGUgZ2lybGllcyBvbiBzdGFuZGJ5IHdhdmluZyBq
dXN0IHRvIHNheSBoaQpEaWQgeW91IHN0b3A/IE5vLCBJIGp1c3QgZHJvdmUg
YnkK"
;; 

let text_list = 
    let open Cryptokit in 
    let open Core in
            transform_string ( Base64.decode() ) text_b64
            |> Core.String.to_list
            |> List.map ~f:(fun c -> int_of_char c )
;;
(*
val text_list : int list =
  [82; 111; 108; 108; 105; 110; 39; 32; 105; 110; 32; 109; 121; 32; 53; 46; 48;
   10; 87; 105; 116; 104; 32; 109; 121; 32; 114; 97; 103; 45; 116; 111; 112;
   32; 100; 111; 119; 110; 32; 115; 111; 32; 109; 121; 32; 104; 97; 105; 114;
   32; 99; 97; 110; 32; 98; 108; 111; 119; 10; 84; 104; 101; 32; 103; 105; 114;
   108; 105; 101; 115; 32; 111; 110; 32; 115; 116; 97; 110; 100; 98; 121; 32;
   119; 97; 118; 105; 110; 103; 32; 106; 117; 115; 116; 32; 116; 111; 32; 115;
   97; 121; 32; 104; 105; 10; 68; 105; 100; 32; 121; 111; 117; 32; 115; 116;
   111; 112; 63; 32; 78; 111; 44; 32; 73; 32; 106; 117; 115; 116; 32; 100; 114;
   111; 118; 101; 32; 98; 121; 10]
*)
let key = intlst_2_str (gen_rand_key_int_list 16)
;;
(* val key : string = "?\021??<f???\r/hyOt?" *)

let key = "?\021??<f???\r/hyOt?" ;;

let intlst_nulls = Core.Array.(create 128 '\000' 
                  |> map ~f:(fun a -> Stdlib.Char.code a) 
                  |> to_list) ;;
(*val intlst_nulls : int list =
  [0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0;
   0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0;
   0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0;
   0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0;
   0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0;
   0; 0; 0]
*)

let plain = intlst_2_str (intlst_nulls @ text_list)
;; 
(* val plain : string =
  "\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000
   \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000
   \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000
   \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000
   \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000
   \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000
   \000\000\000\000\000\000\000\000\000\000\000\000\000\000Rollin' in my 5.0\nW
   ith my rag-top down so my hair can blow\nThe girlies on standby waving just 
   to say hi\nDid you stop? No, I just drove by\n"
 *) 
(* Create a list which comprises a 'size' long run of repeated character, 'chr' *)
let make_runs_lst (chr : char) (size : int) : (int list) =
    Core.Array.(create size chr 
    |> map ~f:(fun a -> Stdlib.Char.code a) 
    |> to_list) ;;


(* Hic est encryption oracle, per exercitio 12. *)
(* The count of unique bytes, via Set module, correlates with the encryption mode
   inasmuch as a larger number indicates CBC.  This works well for plaintext which
   is a uniform stream of a single byte.  The test's reliability decreases as 
   plaintext variability increases.

   I have removed the placement of random ante/post bytes as it seems to me that
   adding them disrupts the attack. 
   
   plain: plaintext string to be encrypted
   key: a string of key
   ante: a list of a single char. prepended to a list of the plain text chars.
   *)
let encryption_oracle_ch12 (plain : string) (key : string) (ante : int list)  =
let open Cpal_simple in
let open Core in
    let intlst = Cpal_simple.str_2_intlst plain in
    let prepended = ante @ intlst in
    let padded = str_2_intlst (do_pad (intlst_2_str (prepended)) '\000') in
    (* ECB *) print_string "ECB: \n" ; 
                     str_2_intlst (encrypt padded key) 

let enc = encryption_oracle_ch12 plain key [];;
(*
Null padding length: 6
ECB: 
val enc : int list =
  [139; 11; 214; 44; 88; 40; 214; 9; 146; 11; 94; 142; 124; 161; 8; 118; 139;
   11; 214; 44; 88; 40; 214; 9; 146; 11; 94; 142; 124; 161; 8; 118; 139; 11;
   214; 44; 88; 40; 214; 9; 146; 11; 94; 142; 124; 161; 8; 118; 139; 11; 214;
   44; 88; 40; 214; 9; 146; 11; 94; 142; 124; 161; 8; 118; 139; 11; 214; 44;
   88; 40; 214; 9; 146; 11; 94; 142; 124; 161; 8; 118; 139; 11; 214; 44; 88;
   40; 214; 9; 146; 11; 94; 142; 124; 161; 8; 118; 139; 11; 214; 44; 88; 40;
   214; 9; 146; 11; 94; 142; 124; 161; 8; 118; 139; 11; 214; 44; 88; 40; 214;
   9; 146; 11; 94; 142; 124; 161; 8; 118; 196; 16; 169; 252; 24; 106; 170; 50;
   253; 32; 178; 19; 52; 150; 86; 144; 160; 226; 44; 105; 56; 125; 164; 79;
   245; 50; 40; 34; 108; 166; 62; 132; 245; 84; 196; 10; 125; 200; 254; 0; 104;
   159; 75; 147; 244; 81; 254; 97; 204; 236; 19; 199; 223; 249; 170; 91; 234;
   43; 116; 20; 27; 192; 215; 239; 38; 12; 153; 221; 84; 41; 84; 129; 30; 156;
   166; 197; 56; 193; 81; 216; 85; 125; 254; 133; 215; 218; 133; 170; 158; 97;
   99; 142; 180; 252; 190; 44; 89; 66; 200; 251; 21; 210; 52; 73; 180; 185;
   132; 150; 52; 239; 125; 98; 234; 8; 12; 153; 170; 231; 75; 222; 141; 25;
   144; 131; 201; 152; 106; 131; 88; 77; 196; 131; 16; 175; 113; 71; 253; 125;
   125; 115; 228; 95; 51; 107]
*)
let enc_set = Base.Set.of_list (module Core.Int) enc;;
(*val enc_set : (int, Core.Int.comparator_witness) Base.Set.t = <abstr> *)

encryption_oracle_ch12 (intlst_2_str text_list) key (make_runs_lst '\101' 0)
;;
(*  The following encrypted output is from the unknown-string, and we note the bytes which
    are produced.
Null padding length: 6
ECB: 
- : int list =
[196; 16; 169; 252; 24; 106; 170; 50; 253; 32; 178; 19; 52; 150; 86; 144; 160;
 226; 44; 105; 56; 125; 164; 79; 245; 50; 40; 34; 108; 166; 62; 132; 245; 84;
 196; 10; 125; 200; 254; 0; 104; 159; 75; 147; 244; 81; 254; 97; 204; 236; 19;
 199; 223; 249; 170; 91; 234; 43; 116; 20; 27; 192; 215; 239; 38; 12; 153; 221;
 84; 41; 84; 129; 30; 156; 166; 197; 56; 193; 81; 216; 85; 125; 254; 133; 215;
 218; 133; 170; 158; 97; 99; 142; 180; 252; 190; 44; 89; 66; 200; 251; 21; 210;
 52; 73; 180; 185; 132; 150; 52; 239; 125; 98; 234; 8; 12; 153; 170; 231; 75;
 222; 141; 25; 144; 131; 201; 152; 106; 131; 88; 77; 196; 131; 16; 175; 113;
 71; 253; 125; 125; 115; 228; 95; 51; 107]

*)

encryption_oracle_ch12 (intlst_2_str text_list) key (make_runs_lst '\101' 1);;
(*  Prepend a single 'A' byte.
Null padding length: 5
ECB: 
- : int list =
[171; 127; 76; 149; 96; 253; 219; 232; 114; 234; 102; 169; 113; 27; 17; 34; 17;
 200; 57; 214; 43; 9; 17; 182; 94; 140; 102; 175; 6; 95; 70; 143; 184; 48; 32;
 57; 209; 207; 219; 84; 190; 178; 181; 205; 176; 132; 175; 39; 5; 250; 61; 140;
 209; 178; 155; 120; 156; 99; 232; 44; 213; 24; 25; 143; 249; 76; 186; 129;
 247; 242; 225; 40; 141; 3; 77; 124; 211; 189; 86; 115; 170; 165; 161; 232; 74;
 181; 161; 45; 151; 86; 83; 27; 49; 232; 58; 175; 127; 222; 247; 26; 169; 68;
 133; 34; 49; 80; 199; 107; 255; 30; 227; 165; 244; 237; 170; 110; 140; 238;
 164; 53; 199; 203; 81; 127; 147; 219; 210; 121; 251; 144; 81; 243; 121; 120;
 10; 30; 63; 4; 67; 163; 111; 69; 90; 239]
*)


encryption_oracle_ch12 (intlst_2_str text_list) key (make_runs_lst '\101' 16);;
(*  But prepend 16 bytes and we get the original encrypted bytes back onto a
    16-byte cut; hence, the block length is 16.

Null padding length: 6
ECB: 
- : int list =
[201; 121; 66; 48; 69; 97; 149; 1; 119; 115; 67; 183; 252; 178; 25; 5; 196; 16;
 169; 252; 24; 106; 170; 50; 253; 32; 178; 19; 52; 150; 86; 144; 160; 226; 44;
 105; 56; 125; 164; 79; 245; 50; 40; 34; 108; 166; 62; 132; 245; 84; 196; 10;
 125; 200; 254; 0; 104; 159; 75; 147; 244; 81; 254; 97; 204; 236; 19; 199; 223;
 249; 170; 91; 234; 43; 116; 20; 27; 192; 215; 239; 38; 12; 153; 221; 84; 41;
 84; 129; 30; 156; 166; 197; 56; 193; 81; 216; 85; 125; 254; 133; 215; 218;
 133; 170; 158; 97; 99; 142; 180; 252; 190; 44; 89; 66; 200; 251; 21; 210; 52;
 73; 180; 185; 132; 150; 52; 239; 125; 98; 234; 8; 12; 153; 170; 231; 75; 222;
 141; 25; 144; 131; 201; 152; 106; 131; 88; 77; 196; 131; 16; 175; 113; 71;
 253; 125; 125; 115; 228; 95; 51; 107]

*)

(* Given an int list, partition the list into a list of N-long int lists *)
let chunky = Core.List.chunks_of ~length:16 enc
;;
(*  Chunking the encryption of 'plain', which has considerable null pre-padding,
    clearly reveals the ECB repetition on a 16-byte cut.
val chunky : int list list =
  [[139; 11; 214; 44; 88; 40; 214; 9; 146; 11; 94; 142; 124; 161; 8; 118];
   [139; 11; 214; 44; 88; 40; 214; 9; 146; 11; 94; 142; 124; 161; 8; 118];
   [139; 11; 214; 44; 88; 40; 214; 9; 146; 11; 94; 142; 124; 161; 8; 118];
   [139; 11; 214; 44; 88; 40; 214; 9; 146; 11; 94; 142; 124; 161; 8; 118];
   [139; 11; 214; 44; 88; 40; 214; 9; 146; 11; 94; 142; 124; 161; 8; 118];
   [139; 11; 214; 44; 88; 40; 214; 9; 146; 11; 94; 142; 124; 161; 8; 118];
   [139; 11; 214; 44; 88; 40; 214; 9; 146; 11; 94; 142; 124; 161; 8; 118];
   [139; 11; 214; 44; 88; 40; 214; 9; 146; 11; 94; 142; 124; 161; 8; 118];
   [196; 16; 169; 252; 24; 106; 170; 50; 253; 32; 178; 19; 52; 150; 86; 144];
   [160; 226; 44; 105; 56; 125; 164; 79; 245; 50; 40; 34; 108; 166; 62; 132];
   [245; 84; 196; 10; 125; 200; 254; 0; 104; 159; 75; 147; 244; 81; 254; 97];
   [204; 236; 19; 199; 223; 249; 170; 91; 234; 43; 116; 20; 27; 192; 215; 239];
   [38; 12; 153; 221; 84; 41; 84; 129; 30; 156; 166; 197; 56; 193; 81; 216];
   [85; 125; 254; 133; 215; 218; 133; 170; 158; 97; 99; 142; 180; 252; 190; 44];
   [89; 66; 200; 251; 21; 210; 52; 73; 180; 185; 132; 150; 52; 239; 125; 98];
   [234; 8; 12; 153; 170; 231; 75; 222; 141; 25; 144; 131; 201; 152; 106; 131];
   [88; 77; 196; 131; 16; 175; 113; 71; 253; 125; 125; 115; 228; 95; 51; 107]]
   *)

let foo = [139; 11; 214; 44; 88; 40; 214; 9; 146; 11; 94; 142; 124; 161; 8; 118];;

Base.List.iter chunky 
    ~f:(fun m -> if (Base.List.equal (fun x y -> x = y) foo m) = true 
        then print_string "* " 
        else print_string "-")
;; 
(* * * * * * * * * ---------- : unit = () *)


let count_members enc_chunks_lst =
Base.List.count enc_chunks_lst
    ~f:(fun m -> (Base.List.equal (fun x y -> x = y) [1;2;3] m))
;; 

(* Work through an 'int list list', deduping consecutive repeats
   as you go. This is a quick & dirty technique to identify
   repeated lists at the start of the cipher, arising when
   prepending padding of a single character buffer. *)
let rec uniques lst =
let open Base in
match lst with
| [] -> []
| [x] -> [x]
| a::b::t -> if Base.List.equal (fun x y -> x = y) a b
                then
                    uniques (b::t)
                else 
                    a :: uniques (b::t) 

;;

List.length chunky;;
(* - : int = 17 *)

List.length (uniques chunky);;
(* - : int = 10 *)

(* Dump a specific index of a list. *)
let dump_internals lst (idx : int) = 
    let ans = List.filteri (fun i a ->  if i = idx then true else false) lst
    in List.hd ans
;;


(* Try prepadding of a length (block-size - 1)  *)
let one_short_A = encryption_oracle_ch12 (intlst_2_str text_list) key (make_runs_lst '\101' 15)
;;
(* We see that byte 16, encrypts to zero, which matches an XOR of 'A' with itself.
Null padding length: 7
ECB: 
val one_short_A : int list =
  [57; 49; 156; 51; 147; 221; 187; 157; 208; 106; 49; 182; 117; 207; 63; 0;
   220; 173; 4; 89; 136; 143; 214; 27; 28; 66; 92; 252; 170; 249; 252; 35; 170;
   139; 184; 119; 85; 158; 39; 129; 41; 129; 192; 117; 0; 50; 243; 118; 128;
   94; 98; 185; 6; 15; 90; 78; 40; 90; 189; 23; 17; 161; 141; 162; 194; 207;
   132; 58; 51; 219; 15; 89; 129; 172; 26; 174; 93; 149; 158; 2; 149; 84; 23;
   136; 146; 110; 238; 162; 186; 78; 200; 132; 217; 94; 172; 249; 4; 83; 32;
   60; 223; 225; 146; 92; 139; 235; 23; 130; 143; 224; 106; 0; 77; 72; 180;
   218; 109; 131; 253; 157; 174; 163; 205; 226; 37; 21; 220; 151; 178; 101;
   244; 108; 5; 18; 166; 244; 133; 38; 41; 174; 123; 109; 155; 126; 99; 240;
   25; 107; 210; 255; 137; 111; 121; 53; 213; 119; 224; 193; 5; 86]
   *)


(* You can add a single byte to the end of the pre-padding like so: *)
let ante_15 last = make_runs_lst '\101' 15 @ List.cons last []
;;

let build_encryption_oracle_assoc_list_ch12 (plain : string) (key : string) (ante : int list) codelist =
let open Cpal_simple in
let open Core in
    let intlst = Cpal_simple.str_2_intlst plain in
    let prepended = ante @ intlst in
    let padded = str_2_intlst (do_pad (intlst_2_str (prepended)) '\000') in
    let enc_lst = str_2_intlst (encrypt padded key) in 
        codelist := (List.take padded 16 , List.take enc_lst 16) :: !codelist
        ; !codelist
;;        


(* String of the decoded plain text; no pre-padding. *)
let plaintext = intlst_2_str (text_list)
;;

let boof (plain : string) (key : string) = 
    let codelist = ref [] in
    for i = 30 to 126 do
        codelist := build_encryption_oracle_assoc_list_ch12 plain key (ante_15 i) codelist
    done;
    !codelist
;;



let assoc_list = boof plaintext key 
;;

int_of_char 'R';;
(* First letter of the plaintext - : int = 82 *)

(* Get the encrypted solution *)
let sol = List.assoc (ante_15 82) assoc_list;;

let plaintext_enc = encryption_oracle_ch12 plaintext key (make_runs_lst '\101' 15)
;;

let chunked_plaintext_enc = Core.List.chunks_of ~length:16 plaintext_enc
;;

Base.List.mem chunked_plaintext_enc sol ~equal:(Base.List.equal (fun x y -> x = y) )
;;

(* See if I can match with the oracle *)
let match_taint_with_encrypted (enc_lst : int list) (ante : int list) (charcode : int) codelist =
    let sol = List.assoc ante codelist 
    and enc_hd = Core.List.take enc_lst 16 in
    if Base.List.equal (fun x y -> x = y) sol enc_hd
        then 
            true
        else 
            false


(* I need to make a new ante_?? function, with variable length paramter, *)

let find_match_char enc_lst =
    let chr = ref ' ' and ans = ref [] in
    let codelist = boof plaintext key in
    for i = 30 to 126 do 
        if ( match_taint_with_encrypted enc_lst (ante_15 i) i codelist)
            then
(*                Printf.printf "Found match in char: %c \n" (char_of_int i)   *)
                ans := [char_of_int i] @ !ans
            else 
                ()
    done 
    ; !ans
;;

find_match_char plaintext_enc
;;

let x : int = 3110



 



