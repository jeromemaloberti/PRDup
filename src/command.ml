type return_status = Error | Finished of (Unix.process_status * string * string)

let read_channel chan =
  let buffer_size = 1024 in
  let buf = Buffer.create buffer_size in
  let s = String.create buffer_size in
  let rec aux buf s =
    let chars_read = input chan s 0 buffer_size in
    Buffer.add_substring buf s 0 chars_read;
    if chars_read <> 0 then aux buf s
  in 
  aux buf s;
  Buffer.contents buf
    
let log_file log_path out =
  match log_path with
      None -> ()
    | Some log -> 
        (* prerr_endline ("logging in " ^ log); *)
        let ch = open_out_gen [Open_wronly; Open_append; Open_creat] 0o644 log in
        output_string ch out;
        close_out ch
          
let execute ?(env=[| |]) ?log_path path cmd =
  prerr_endline ("Executing " ^ cmd ^ " in " ^ path);
  let curpath = Unix.getcwd () in
  try
    Unix.chdir path;
    let ic, oc, ec = Unix.open_process_full cmd env in
    let out = read_channel ic in
    let err = read_channel ec in
    let exit_status = Unix.close_process_full (ic, oc, ec) in
    Unix.chdir curpath;
    log_file log_path out;
    log_file log_path err;
    Finished (exit_status,out,err)
  with 
  | e -> 
    Unix.chdir curpath;
    Printexc.print_backtrace stderr;
    Error

let run ?(env=[| |]) path cmd  =
  let env = Array.append env
		(Unix.environment ()) in
  let get_error_code = function
    | Unix.WEXITED r -> r
    | Unix.WSIGNALED r -> r
    | Unix.WSTOPPED r -> r in      
  let res = execute ~env ~log_path:"/tmp/prdup.log" path cmd in
  match res with
    Finished (Unix.WEXITED 0,_,_) -> () (* OK !! *)
  | Error -> failwith (cmd ^ " : Error")
  | Finished (r,_,_) -> 
    failwith (cmd ^ " : Failed with code " ^ (string_of_int (get_error_code r)))
