(* Generate a hex value from a char. *)
(* #require "iter";; *)

module HexCraft =
  struct
        let rec pairs l =
        match l with
          | [] -> []
          | [_] -> []
          | a::b::[] -> (a,b) :: pairs []
          | a::b::tl -> (a,b) :: pairs tl
         
        let hexchar2int c =
          match c with
          | '0' .. '9' -> Stdlib.Char.code c - 48
          | 'a' .. 'f' -> Stdlib.Char.code c - 87
          | 'A' .. 'F' -> Stdlib.Char.code c - 55
          | x          -> Stdlib.Char.code c

        let int2hexchar i =
          match i with
          | i when i >= 0 && i <= 9 -> Stdlib.Char.chr (i+48)
          | i when i >= 10 && i <= 15 -> Stdlib.Char.chr (i+87)
          | _  -> Stdlib.Char.chr i

        let i2hxch lst =
          Iter.(of_list lst |> map (fun x -> int2hexchar x) |> to_list);;

        let hxch2i lst =
          Iter.(of_list lst |> map (fun x -> hexchar2int x) |> to_list);;

        let cram a b = (a lsl 4) lxor b

        let rec combine lst =
        match lst with 
        | [] -> [] 
        | [x] ->  [x]
        | a::b::[] -> cram a b :: combine []
        | a::b::t  -> cram a b :: combine t

  end

;;

