


(* Get a list of hex strings from the input file. *)
(* This makes a list of strings, separated by a newline character. *)
let (cipher_strings : string list) = 
    let open Cryptokit in 
    let open Core in
        In_channel.read_lines "./8.txt"
;;
(*
 ["8a10247f90d0a05538888ad6205882196f5f6d05c21ec8dca0cb0be02c3f8b09e3...221b58170e633381e3847c6b1c28dda2913c011e13fc4406f8f"... (* string length 320; truncated *);
   "bd20aad820c9e387ea57408566e5844c1e470e9d6fbbdba3a6b4df1dd85bce220...ce09823bab5aee947a682bb3156f42df1d8dc320a897ee79981"... (* string length 320; truncated *);
   "ed9eccbe79394ca0575a81d1fa51443aa3e83e5e3cdb7a4c5897faa6b4ac123c1...379abf3e189f2ecf5f997db17db69467bf6dfd485522d084d6e"... (* string length 320; truncated *);
   .
   .
   .
      "95b3a1adbcf8d2987c2f2cba58d5f0a4ef6e0301e186b8d62a59acb6eb4be54867136f319f97470fedae743acd6bb0231"... (* string length 320; truncated *);
   "a6cadd53a2621482b7d66ecc82dc4ea6431bc0191c3801ac6b705df38c7fffe469043e5002096aca4aaca5ef033cc2c5"... (* string length 320; truncated *);
   "06df04188832b10afff94209d2aa1c8a123702de28234dcd3e0a7d36c1aa8449e6fa55e3e1e3d77d8424e87a45e3869"... (* string length 320; truncated *)]
*)

(* You can chunk lists. Output is a string list list, with member lists partitioned into
16-byte chunks. *)
let rec chunk16 (cyphr : string list) : ('a list) = 
    let open Core in
    let f x = (String.to_list x |> List.chunks_of ~length:16) in
    match cyphr with
    | [] -> []
    | [x] -> f x :: []
    | h :: t -> f h :: (chunk16 t)
;;


let rec amake_strings_list (lst: (char list list list)) : (string list list) =
let open Core in
    let f x = String.of_char_list x in
    match lst with
    | [] -> []
    | [l] -> Stdlib.List.map f l :: []
    | h :: t -> Stdlib.List.map f h :: (amake_strings_list t)
;;

let all_lists = amake_strings_list ( chunk16 cipher_strings )
;;
(*
val all_lists : string list list =
  [["8a10247f90d0a055"; "38888ad620588219"; "6f5f6d05c21ec8dc";
    "a0cb0be02c3f8b09"; "e382963f443aa514"; "daa501257b09a36b";
    "f8c4c392d8ca1bf4"; "395f0d5f2542148c"; "7e5ff22237969874";
    "bf66cb85357ef999"; "56accf13ba1af36c"; "a7a91a50533c4d89";
    "b7353f908c5a1667"; "74293b0bf6247391"; "df69c87dacc4125a";
    "99ec417221b58170"; "e633381e3847c6b1"; "c28dda2913c011e1";
    "3fc4406f8fe73bbf"; "78e803e1d995ce4d"];
   ["bd20aad820c9e387"; "ea57408566e5844c"; "1e470e9d6fbbdba3";
    "a6b4df1dd85bce22"; "08f1944f1827d015"; "df9c46c22803f41d";
    "1052acb721977f0c"; "cc13db95c9702520"; "91ea5e36e423ee6a";
    "2f2d12ef909fcadd"; "42529885d221af12"; "25e32161b85e6dc0";
    "3465cf398c937846"; "b18bac05e88820a5"; "67caac113762753d";
    "ffbe6ece09823bab"; "5aee947a682bb315"; "6f42df1d8dc320a8";
    "97ee79981cf93739"; "0b4ae93eb8657f6c"];
   ["ed9eccbe79394ca0"; "575a81d1fa51443a"; "a3e83e5e3cdb7a4c";
    "5897faa6b4ac123c"; "1dde2dff4d7c5b77"; "d2aa3c090cebce70";
    "340e7f0e0b754ca2"; "6b9c108ca0dbfdcd"; "8aa230eb9420654b";
    "252ffc118e830179"; "cc12b64b2819f81e"; "dcc2543d759c422c";
    "3850051d543c9bc1"; "dcda7c2a6c9896e1"; "161d61c3c13c80ee";
    "79c08379abf3e189"; "f2ecf5f997db17db"; "69467bf6dfd48552";
    "2d084d6e00a32952"; "6848bb85414a7e6a"];
   ["4c0a7faa755a8b87"; "7860d60f62e4a85e"; "7319b39628d50921";
    "1a00026d4d278dd7"; "746c6feb6bf232d3"; "8298a321e48f8172";
    "eadb7782e188e134"; "2bf088814329608a"; "e756700d7786ab99";
    "602e83ab084407e0"; "5c349a0d72ee7662"; "005784694d98fdf1";
    "d2e08efce71d14d9"; "40e7c4105d3fa509"; "5454fe478401ba38";
    "f98a4eebd38477c6"; "13972c86f08e69f9"; "e82e1ac09e67d812";
    "38271bb492da526b"; "b1897bd330fe7b75"];
   ["dd6a92b0659968f0"; "1d7b638960d747f7"; "f0a0b20460de239b";
    "8f16d5a95936d1a4"; "d9f4d3c77f5b1494"; "2d91304ce572dab5";
    "4c8e4c01cab0df8d"; "7653f0ef9fd48619"; "1a0ead2f1cfa71b3";
    "0d7653322fde828b"; "83f4ffafd2060727"; "acf2c0d4062ed24f";
    "c9608bae7ab851cc"; "e4fde56d4ad593ed"; "775ce856d7299e17";
    "d5f88325dddf7e26"; "8534710d3510ed24"; "093d217f199afdb6";
    "50581ac7962d0e28"; "1469e040beae01e1"];
   ["b1f6508b1321e924"; "066febfe8030a908"; "e8086eb5ac423895";
    "b74ae91b9cea65e9"; "d4249057b23b970e"; "23f0b87b641b98cb";
    "c5fb7438a2844fd9"; "49a937f05f767046"; "2266c3927177a2bf";
    "3c5873695ba9334c"; "0d57e749e2132df5"; "86899c88827cad98";
    "efc7c9d74f001f57"; "b31d3826e4448067"; "ff43b2ab045a7123";
    "72ce8f8a229e8452"; "89ecaa2e038eefc9"; "ef4a710509f4ea14";
    "e695cf44977cbfbb"; "1a9d806e43fe1af7"];
   ["cc230392928a4146"; "fe2dfea83e256792"; "3bb2676de8b879e1";
    "2cfaa09379bef6e5"; "99aec3187be62a73"; "35b90c2692477248";
    "80835747e302068a"; "3325ed3a02feedd5"; "c129aa8d2846db88";
    "e29ae896a8d93554"; "83a54bbd70e1ed62"; "8c78a8eae97a3518";
    "f33b6d9c4d04ef3b"; "f79df7cd1042eaf2"; "09d6d52a9f1f293a";
    "e3a699a3a3dbad77"; "a9cc0d4b0b47db49"; "397949b61ba6be52";
    "a140a3dc7dee41d1"; "06609df433d587c3"];
   ["da56b86bb13657dd"; "f180fec84bc8070f"; "ac27fded7d3d659d";
    "7951d1d7bd2b3c01"; "5463c2118fc4be14"; "cb3c21f1249a6509";
    "d5b409d3181c447f"; "0146ea80efc324fa"; "5484d4fc6be03799";
    "3d75d4f5deeb33ca"; "0c71fca4fcc19ba6"; "b6d8900ef0432e15";
    "d82a1a9e494f2ef1"; "2d4d3e3f1ec40f5c"; "e75a6b0b2fd21656";
    "aae26e47af8ae60d"; "5fba3dd86c9fe2dd"; "116f8f443408c004";
    "ee826168dd888b35"; "08b4eba633eddb60"];
   ["855ad6a9215ba9db"; "1eb2fc024fcafd9a"; "f2d31b81a60341b6";
    "024231cb1a6a1e29"; "1598cc886f042851"; "c8f218e01a571419";
    "32b6cdbf3f19fe70"; "5a7fb7db0c9833f4"; "9a30906fe7d5e2a1";
    "78fe4b92e089829e"; "7c14da0659ce7644"; "1e0c17a54acd7b38";
    "40769496868907e8"; "3e9d2fb9fd2dbf1a"; "230a0ee15eb978f2";
    "d6e12f9d63a686bf"; "0361503a4e4234e8"; "984c68b0e0882f3a";
    "0261bbe1d248f4c1"; "07ce2453ae63cbe6"];
   ["abfb8e76094d4c27"; "a44f3170271c9c0e"; "312c192f42bc9597";
    "c928d8c9e461f5e2"; "ffb555283e4c0791"; "4f82dc6dae5e8f3b";
    "3d4dd5cb3ab1ce8f"; "0d8cde59ad92d5db"; "56e2c6f584c1ede4";
    "a31403141ba42b18"; "528d501f62c5cad6"; "950c8d8f14c1c443";
    "9dcc27f4f20cecb1"; "d7559758d5080d88"; "fc7ec9045a201442";
    "027baad56ae952c5"; "be100b55291d8437"; "20c92d10547022b1";
    "2505e2000084ec72"; "b069afa60f15258c"];
   ["0471984bac512f7b"; "486ddd641cc31823"; "e66c7050ccec2ac7";
    "1cf85f3493b32d87"; "76e9d486ec29f8ab"; "568451c8b60527d5";
    "2b15d152db1b072a"; "cce870e3ff3541d8"; "14c52bc1393e4167";
    "97a423c88d156370"; "4ec8c16d82032006"; "3fddb89592d5c2a2";
    "4e978fd5fa44fd0f"; "8f76821b359caf04"; "1edc75f06c235ddc";
    "2561198553f1cdbe"; "ed11ec62cc9af292"; "35ff619caede8467";
    "9bb25f520313543b"; "ecbb79becbfdd509"];
   ["629f595ea8e740e8"; "a3505770ef70d3a6"; "ab74b17c6879a198";
    "3430e607b04c2326"; "57b8664557455b04"; "32bc8c5913d520c3";
    "e82e29aede851d30"; "bb2796216edf3add"; "ebe34afdf478d760";
    "77429006fdfcdc26"; "dc054c5829c08ccc"; "dc330522a5931683";
    "7b53cf5c38bf84d5"; "ab1a536371e50d18"; "7f2aecb260ce79ca";
    "ffc3ec77a4bf7698"; "174e1fcd46356481"; "44e2d0d983715656";
    "f6efda616271f6ba"; "af9d79ff8ee7c017"];
   ["d72574d26e0273ac"; "584fdc8e149196b5"; "28b3c8fbbe053616";
    "b5017e5638cd6ec8"; "6119712e32ecff61"; "24ed3ef268d6a80f";
    "3e7e9fe9f7e891ac"; "f29392fd0391b77c"; "0da13559c9aa963e";
    "c91882ba1e53d81d"; "07056eef4535776e"; "894154e07edfd0ef";
    "d53dbff0a1ceaca4"; "7aeca4bdfd733d7a"; "262a67101b691862";
    "d773c15e90254786"; "e28ac66e559eaf75"; "6ca2ba342fcf07ee";
    "14c011998683e335"; "c4f3ec60c9c15f15"];
   ["6037304b0a7baad8"; "14a3b2ef8fa0be71"; "beef741ab7278553";
    "7e82b844609b36fc"; "07822abc20bc28af"; "eb8fc92fe47aa516";
    "4d1b7f89039094ee"; "894d24f0fae9b32b"; "14008033361f90c0";
    "b1b748bd555a002"... (* string length 16; truncated *);
    "d2203f1949b082"... (* string length 16; truncated *);
    "1e2c7039d4508"... (* string length 16; truncated *);
    "15dc7b5c7538"... (* string length 16; truncated *);
    "07b7fe232b1"... (* string length 16; truncated *);
    "b7dac6e8ca"... (* string length 16; truncated *);
    "1e12c868e"... (* string length 16; truncated *);
    "7567cca1"... (* string length 16; truncated *);
    "d5c1573"... (* string length 16; truncated *);
    "d60eae"... (* string length 16; truncated *);
    "77aa0"... (* string length 16; truncated *)];
   ["251"... (* string length 16; truncated *);
    "fc"... (* string length 16; truncated *);
    "9"... (* string length 16; truncated *);
    ""... (* string length 16; truncated *); ...];
   ...]
   *)



(* Create a Set of type string. *)
module SS = Set.Make(String)
;;

(* Print each set member to console. *)
let print_set s = 
     SS.iter print_endline s;;

(* .of_list works when SS has been created. *)
(* Create a list of sets. *)
let rec create_set_list (slst : string list list) (out : 'a list) : ('a list) =
let open Stdlib in 
    match slst with
    | [] -> out
    | [x] -> SS.of_list x :: out
    | h :: t -> create_set_list t (SS.of_list h :: out)
;;

let sets = create_set_list all_lists []
;;

(*
val sets : SS.t list =
  [<abstr>; <abstr>; <abstr>; <abstr>; <abstr>; <abstr>; <abstr>; <abstr>;
   <abstr>; <abstr>; <abstr>; <abstr>; <abstr>; <abstr>; <abstr>; <abstr>;
   <abstr>; <abstr>; <abstr>; <abstr>; <abstr>; <abstr>; <abstr>; <abstr>;
   <abstr>; <abstr>; <abstr>; <abstr>; <abstr>; <abstr>; <abstr>; <abstr>;
   <abstr>; <abstr>; <abstr>; <abstr>; <abstr>; <abstr>; <abstr>; <abstr>;
   <abstr>; <abstr>; <abstr>; <abstr>; <abstr>; <abstr>; <abstr>; <abstr>;
   <abstr>; <abstr>; <abstr>; <abstr>; <abstr>; <abstr>; <abstr>; <abstr>;
   <abstr>; <abstr>; <abstr>; <abstr>; <abstr>; <abstr>; <abstr>; <abstr>;
   <abstr>; <abstr>; <abstr>; <abstr>; <abstr>; <abstr>; <abstr>; <abstr>;
   <abstr>; <abstr>; <abstr>; <abstr>; <abstr>; <abstr>; <abstr>; <abstr>;
   <abstr>; <abstr>; <abstr>; <abstr>; <abstr>; <abstr>; <abstr>; <abstr>;
   <abstr>; <abstr>; <abstr>; <abstr>; <abstr>; <abstr>; <abstr>; <abstr>;
   <abstr>; <abstr>; <abstr>; <abstr>; <abstr>; <abstr>; <abstr>; <abstr>;
   <abstr>; <abstr>; <abstr>; <abstr>; <abstr>; <abstr>; <abstr>; <abstr>;
   <abstr>; <abstr>; <abstr>; <abstr>; <abstr>; <abstr>; <abstr>; <abstr>;
   <abstr>; <abstr>; <abstr>; <abstr>; <abstr>; <abstr>; <abstr>; <abstr>;
   <abstr>; <abstr>; <abstr>; <abstr>; <abstr>; <abstr>; <abstr>; <abstr>;
   <abstr>; <abstr>; <abstr>; <abstr>; <abstr>; <abstr>; <abstr>; <abstr>;
   <abstr>; <abstr>; <abstr>; <abstr>; <abstr>; ...]
   *)


(* Print out the cardinality of each set, noting which is the odd one out. *)
(* Sets remove duplicates, which is why this method works - it dedupes cipher repeats. *)

List.iter (fun x -> Printf.printf "%i " (SS.cardinal x) ) sets
;;
(*
20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 
20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 
20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 14 <------  Woot!
20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 
20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 
20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 
20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 
20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 
20 20 20 20 20 20 20 20 20 20 20 20 - : unit = ()
*)


let select_low_cardinality_set sets =
let open Stdlib in
    List.iter (fun x -> if SS.cardinal x < 20 then print_set x else () ) sets
;;

select_low_cardinality_set sets
;;
(*
08649af70dc06f4f
1f2a3f9c4040deb0
66489154789a6b03
7840a8a31c810a3d
9475c9dfdbc1d465
97949d9c7e82bf5a
97a93eab8d6aecd5
9d11b0348542bb57
ab51b29933f2c123
c58386b06fba186a
d403180c98c8f6db
d5d2d69c744cd283
d880619740a8a19b
e2dd052f6b641dbf
- : unit = ()
*)

(* Now that we know which set contains ECB encrypted bytes, search for the 
   original list which the set was created from.  Note that sets output their
   elements in order, smallest to largest, which disrupts the ciphertext. *)
let ecb = List.find (fun n -> List.mem "08649af70dc06f4f" n) all_lists
;;

(*
- : string list =
["d880619740a8a19b"; "7840a8a31c810a3d"; "08649af70dc06f4f";
 "d5d2d69c744cd283"; "e2dd052f6b641dbf"; "9d11b0348542bb57";
 "08649af70dc06f4f"; "d5d2d69c744cd283"; "9475c9dfdbc1d465";
 "97949d9c7e82bf5a"; "08649af70dc06f4f"; "d5d2d69c744cd283";
 "97a93eab8d6aecd5"; "66489154789a6b03"; "08649af70dc06f4f";
 "d5d2d69c744cd283"; "d403180c98c8f6db"; "1f2a3f9c4040deb0";
 "ab51b29933f2c123"; "c58386b06fba186a"]
*)

