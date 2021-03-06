open Stdint
open Core

  let a = Stdlib.Char.code 'a'
  let b = Stdlib.Char.code  'b'
(* val a : int = 97
    val b : int = 98  
*)
;;

(* Hamming number is the count of active bits in a binary number. *)
let rec hamming byt =
match byt with 
| 0 -> 0
| b -> (b land 1) + hamming (b lsr 1)

(* Take two bytes, XOR them, and calculate the hamming number. *)
let ham_of_xored_bytes a b =
let aa = Uint8.of_int (Stdlib.Char.code a) and bb = Uint8.of_int (Stdlib.Char.code b) in 
    let ham = Uint8.logxor aa bb |> Uint8.to_int |> hamming
in ham
;;

(* The given start example in Challenge 1_6 *)
let wok = "wokka wokka!!!" and this = "this is a test" in
let woklst = String.to_list wok and thislst = String.to_list this in
let start = ham_of_xored_bytes '\000' '\000' in 
let res = List.fold2_exn woklst thislst ~init:start
    ~f:(fun s l1 l2 -> s + (ham_of_xored_bytes l1 l2  ) ) 
    in res
(* - : int = 37 *)

let cipher_string = In_channel.read_all "./6.txt"
;;

let ans cipherstr =
let ham_lst = ref [] in
for keysz = 1 to 40 do
(* let keysz = 5 in  *)
   let fst = Stdlib.String.sub cipherstr 0 keysz and snd = Stdlib.String.sub cipherstr keysz keysz
  in
    let flst = String.to_list fst and slst = String.to_list snd in
    let start = ham_of_xored_bytes '\000' '\000' in 
    let ham = List.fold2_exn flst slst ~init:start
        ~f:(fun s l1 l2 -> s + (ham_of_xored_bytes l1 l2  ) ) in
    let norm =  ( float_of_int ham /. float_of_int keysz ) in 
    ham_lst := (keysz, norm) :: !ham_lst ;
    printf "   %3.2f " norm; 
    done;
    !ham_lst


let get_ham_list = ans cipher_string

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

let print_pair x = 
    match x with 
    | Some (k,v) -> Printf.printf "key length: %i  |hamming|: %f\n" k v
    | None -> print_string "Empty keyval\n"

;;

let sorted = sort get_ham_list in
    let keyval = List.hd sorted in 
        print_pair keyval; keyval
;;

let arr = String.to_array cipher_string in 
    let matrix = Array.make_matrix 3 1300 arr in
        let transpose = Array.transpose matrix in transpose

;;
(* This might be fruitful? *)
let ninestr = "123456789"
let ninearr = String.to_array ninestr;;

(* - : char array = [|'1'; '2'; '3'; '4'; '5'; '6'; '7'; '8'; '9'|]  *)

open Bigarray

let bigarr = Array1.of_array Char c_layout ninearr;;
let genarray = genarray_of_array1 bigarr;;
let array2d = reshape genarray [|3;3|];;

(* val bigarr : (char, int8_unsigned_elt, c_layout) Array1.t = <abstr>  *)
Array1.size_in_bytes bigarr;;
(* - : int = 9 *)
Genarray.dims array2d;;

let slice03 = Genarray.slice_left array2d  [|0|];;
Genarray.get slice03 [|0|];;  (* 1 *)
Genarray.get slice03 [|1|];;  (* 2 *)
Genarray.get slice03 [|2|];;  (* 3 *)

let slice13 = Genarray.slice_left array2d  [|1|];;
Genarray.get slice13 [|0|];;  (* 4 *)
Genarray.get slice13 [|1|];;  (* 5 *)
Genarray.get slice13 [|2|];;  (* 6 *)

let slice23 = Genarray.slice_left array2d  [|2|];;
Genarray.get slice23 [|0|];;  (* 7 *)
Genarray.get slice23 [|1|];;  (* 8 *)
Genarray.get slice23 [|2|];;  (* 9 *)

let arrsublft = Genarray.sub_left array2d 0 1;;
Genarray.dims arrsublft;;
(* - : int array = [|1; 3|]  *)

Genarray.get arrsublft [|0;0|];;  (* 1 *)
Genarray.get arrsublft [|0;1|];;  (* 2 *)
Genarray.get arrsublft [|0;2|];;  (* 3 *)

