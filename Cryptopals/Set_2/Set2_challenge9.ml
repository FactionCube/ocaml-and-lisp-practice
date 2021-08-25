

(* produce a padded key char list *)
let do_pad =
    let open Core in
        let key = String.to_list "YELLOW SUBMARINE" 
        and tail = Array.to_list (Array.create 4 '\004') in
        let padded = key @ tail 
        in padded
;;
(*
val do_pad : char list =
  ['Y'; 'E'; 'L'; 'L'; 'O'; 'W'; ' '; 'S'; 'U'; 'B'; 'M'; 'A'; 'R'; 'I'; 'N';
   'E'; '\004'; '\004'; '\004'; '\004']
   *)