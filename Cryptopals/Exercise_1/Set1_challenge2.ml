#require "core";;
#require "iter";;

open HexCraft

let a = "1c0111001f010100061a024b53535009181c" and b = "686974207468652062756c6c277320657965"

(* val ans : string = "746865206b696420646f6e277420706c6179"  *)

let i2hxch lst = 
  Iter.(of_list lst |> map (fun x -> HexCraft.int2hexchar x) |> to_list);; 

let hxch2i lst = 
  Iter.(of_list lst |> map (fun x -> HexCraft.hexchar2int x) |> to_list);; 

let computeXor sa sb =
let sal = hxch2i (Core.String.to_list sa) and sbl = hxch2i (Core.String.to_list sb) in 
let xor = Core.List.map2_exn ~f:(fun a b -> a lxor b) sal sbl in
i2hxch xor |>Core.String.of_char_list
in computeXor a b

;;

