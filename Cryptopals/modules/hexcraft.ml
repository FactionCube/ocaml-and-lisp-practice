(* Generate a hex value from a char. *)
(* #require "iter";; *)

module HexCraft =
  struct
        (* Take a list a convert it to a list of pairs. *)
        let rec pairs l =
        match l with
          | [] -> []
          | [_] -> []
          | a::b::[] -> (a,b) :: pairs []
          | a::b::tl -> (a,b) :: pairs tl

        (* Convert a hexadecimal character to its ASCII hex value. *) 
        let hexchar2int c =
          match c with
          | '0' .. '9' -> Stdlib.Char.code c - 48
          | 'a' .. 'f' -> Stdlib.Char.code c - 87
          | 'A' .. 'F' -> Stdlib.Char.code c - 55
          | x          -> Stdlib.Char.code c

        (* Convert ASCII hex value to its hexadecimal character. *)
        let int2hexchar i =
          match i with
          | i when i >= 0 && i <= 9 -> Stdlib.Char.chr (i+48)
          | i when i >= 10 && i <= 15 -> Stdlib.Char.chr (i+87)
          | _  -> Stdlib.Char.chr i

        (* Generate a list of hex chars from a list of integers. *)
        let i2hxch lst =
          Iter.(of_list lst |> map (fun x -> int2hexchar x) |> to_list);;

        (* Generate an integer list from a list of hex chars. *)
        let hxch2i lst =
          Iter.(of_list lst |> map (fun x -> hexchar2int x) |> to_list);;

        (* a b -> ab, logically. *)
        let cram a b = (a lsl 4) lxor b

        (* Generate a list of hex character pairs from a list of consecutive hex characters. *)
        let rec combine lst =
        match lst with 
        | [] -> [] 
        | [x] ->  [x]
        | a::b::[] -> cram a b :: combine []
        | a::b::t  -> cram a b :: combine t

  end

;;

