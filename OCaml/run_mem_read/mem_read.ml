(* mem_read.ml *)

let run_ps process_name =
  let cmd =
  Filename.quote_command "ps" 
                         ~stdout:"./local.run_ps.stdout" 
                         ~stderr:"./local.run_ps.stderr"
                         ["--no-headers"; "-C"; process_name; "-o"; "pid"]
                        in
      match Sys.command cmd with
      | 0 -> ()
      | n ->  Fmt.failwith
              "User warning:\n\tFailed to set up the test environment: command `%s' exited with \
              non-zero exit code %d"
              cmd n
    
(* Collect the process name to examine. *)
let filename_param =
  let open Core.Command.Param in anon ("filename" %: string)



let command =
  let open Core in 
  Command.basic
    ~summary: "Gather the PROCESS ID of a given, running process name."
    ~readme:(fun () -> "Per exempla:\n\t'mem_read boinc'\n")
    Command.Param.(
      both
      (anon ("process-name" %: string))
      (anon ("map-segment" %: string))
      |> map ~f:(fun (proc, s) -> (fun () -> Printf.printf "Searching for process: %s\n" proc;
                                             Printf.printf "Section to extract: %s\n" s; 
                                            run_ps proc)))

let () =
  let aux =
    Core.Command.run ~version:"1.0" ~build_info:"mem_read" command
  in aux


(* open an in_channel on a file. *)
let cat_stdout infile = 
  let errstat = Unix.stat infile in
    if Sys.file_exists infile && (errstat.Unix.st_size <> 0)
                then Core.Unix.open_process_in ("cat "^infile^" 2> /dev/null")
                else failwith "No viable input file."


(* open an in_channel on the output file run by 'let () ' above. *)
let ic_stdout = cat_stdout "./local.stdout"

(* gather the content of the in_channel file, i.e. the PID. *)
let get_pid boinc_ic =
  match boinc_ic with
  | x -> 
    try 
      (int_of_string (String.trim (Core.In_channel.input_all x)) )
    with
      Failure _ -> -99
  
(* convert 'Some x' to just 'x'. *)  
let un_option (x: int option) : int = 
  match x with
  | Some a -> a
  | None -> failwith "Looks like the option you handed me was None."
;;

let pid = get_pid ic_stdout

let check_pid (pid' : int) : unit = 
  if pid' = -99 then failwith "Bad PID value" else Printf.printf "PID is: %d\n" pid'
  
let () = check_pid pid

(* We have a valid PID, so import the /proc/pid/map file into a list. *)
let get_maps_lst (pid' : int) : string list = 
  let pid_string = string_of_int pid' in 
    Core.In_channel.read_lines ("/proc/"^pid_string^"/maps")

let inlst = get_maps_lst pid
let myregex = Re.Posix.compile_pat {|([0-9A-Fa-f]+)-([0-9A-Fa-f]+) ([-r])([-wxp0-9A-Fa-f :]+)  ([]a-z-A-Z0-9._/( )[]+)|}

let get_subs regex map_line = Re.exec regex map_line

let g subs = let triple = (Re.Group.get subs 1, Re.Group.get subs 2, Re.Group.get subs 5) in triple

let rec get_mem_regions regex lst f = 
  (*let open Re in*)
  match lst with
  | [] -> []
  | [x] -> (f (get_subs regex x) )  :: get_mem_regions regex [] g
  | h::t -> (f (get_subs regex h) )  :: get_mem_regions regex t g

let is_readables_opt subs n =
  (*let open Re in*)
  match subs with
    | Some r -> if String.equal (Re.Group.get r n) "r" then true else false
    | None -> false  
let readables = List.filter (fun x -> if x != "" then is_readables_opt (Re.exec_opt myregex x ) 3 else false) inlst

let triples = get_mem_regions myregex readables g

(* Select some segment of the memory map. *)
let heap_seg = List.filter (fun (_,_,x) -> String.equal x Sys.argv.(2)) triples;;

(* Now that we have collected data from the maps file, let's look into extracting memory 
  from the running process. *)

(* unused since the tuples were small. *)  
type couple = { a : int; b: int }

let fst (a,_,_) = a and snd (_,b,_) = b and one (a,_) = a and two (_,b) = b

let skip_and_count a_triple = 
  let skip = int_of_string ("0x"^(fst a_triple)) 
  and lst_str = int_of_string ("0x"^(snd a_triple)) in
  (skip,(lst_str - skip))

let sk_ct = skip_and_count (List.hd heap_seg)
 
let () =
Printf.printf "[heap] memory range is: %x %x\n" ( one sk_ct) (two sk_ct)

let () =
let first = one sk_ct and pid' = string_of_int pid and proc = Sys.argv.(1) and seg = Sys.argv.(2) in
let skip = "skip=" ^ (string_of_int first) in
let count = "count=" ^ (string_of_int (two sk_ct)) in
let local_cmd =
  Filename.quote_command "dd" ~stderr:"./local.stderr"
    [ "if=/proc/"^pid'^"/mem"; "bs=1"; skip; count; "status=none"; "of=./"^proc^"."^seg^".bin" ]
    in
      match Sys.command local_cmd with
      | 0 -> ()
      | n -> Fmt.failwith "Failed to set up the test environment: command `%s' exited with non-zero exit code %d" local_cmd n
