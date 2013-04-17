open Lwt
open Printf
open Github_t
open Github_j
open Github


let api = "https://api.github.com"

let create_pull_request ~title ~description ~user ~branch_name ~dest_branch ~repo ~token ~caller =
  let pull = { pull_request_title = title; pull_request_body = Some description;
	       pull_request_base = dest_branch; pull_request_head = (caller ^ ":" ^ branch_name) } in
  let body = string_of_pull_request pull in
  let uri =  Uri.of_string (Printf.sprintf "%s/repos/%s/%s/pulls" api user repo) in
  API.post ~body ~uri ~token ~expected_code:`Created (fun body -> return body)
  
let prepare_git_repo ~dest_branch ~user ~repo ~shas ~branch_name ~caller =
  let repo_path = "/tmp/" ^ repo in
  let cherry_pick sha = Command.run repo_path ("git cherry-pick " ^ sha) in
  try 
    Command.run "/tmp" ("git clone -b " ^ dest_branch ^ " git@github.com:xen-org/" ^ repo ^ ".git");
    if caller <> user then
      Command.run repo_path ("git remote add " ^ user ^ " git://github.com/" ^ user ^ "/" ^ repo ^ ".git");
    Command.run repo_path ("git remote add " ^ caller ^ " git@github.com:" ^ caller ^ "/" ^ repo ^ ".git");
    Command.run repo_path ("git fetch " ^ user);
    Command.run repo_path ("git checkout -b " ^ branch_name);
    List.iter (fun sha -> cherry_pick sha) shas;
    Command.run repo_path ("git push " ^ caller ^ " " ^ branch_name)
  with Failure s -> prerr_endline s;
    ()
      
let get_token ~user ~pass =
  let r = Github.Token.create ~user ~pass () in
  lwt auth = Github.Monad.run r in
  let token = Github.Token.of_auth auth in
  prerr_endline (Github.Token.to_string token);
  return token

let pullrequest_commits ~user ~repo ~issue_number =
  Uri.of_string (Printf.sprintf "%s/repos/%s/%s/pulls/%d/commits" api user repo issue_number)

let get_pullrequest ~token ~repo ~user ~issue_number =
  let uri = URI.repo_issue ~user ~repo ~issue_number in
  API.get ~token ~uri (fun b -> return (issue_of_string b))
    
let get_pullrequest_commits ~token ~repo ~user ~issue_number =
  let uri = pullrequest_commits ~user ~repo ~issue_number in
  API.get ~token ~uri (fun b -> return (repo_commits_of_string b))

let pr_info ~user ~pass ~issue_number ~dest_branch ~repo ~branch_name =
  lwt token = get_token ~user ~pass in
  lwt r = 
    let open Github.Monad in
    run (
      get_pullrequest ~token ~user:"xen-org" ~repo ~issue_number >>=
	fun pr ->
      eprintf "pullrequest %s user %s\n" pr.issue_title pr.issue_user.user_login;
      get_pullrequest_commits ~token ~user:"xen-org" ~repo ~issue_number >>=
	fun cs -> let shas = List.map (fun c -> eprintf "commit: %s\n" c.repo_commit_sha; c.repo_commit_sha) cs in
		  prepare_git_repo ~dest_branch ~user:pr.issue_user.user_login ~repo ~shas 
		    ~branch_name ~caller:user;
		  create_pull_request ~title:pr.issue_title ~description:pr.issue_body ~user:"xen-org"
		    ~branch_name ~dest_branch ~repo ~token
		    ~caller:user >>= fun body -> prerr_endline body; 
		  return ()
    ) in
  return ()

let read_line_no_echo ?pre () =
	let open Unix in
	let fd = stdout in
	let tio_old = tcgetattr fd in
	let tio_new = {tio_old with c_echo = false} in
	tcsetattr fd TCSANOW tio_new ;
	(match pre with
	| None -> ()
	| Some s -> Printf.printf "%s" s) ;
	let str = read_line () in
	tcsetattr fd TCSANOW tio_old ;
	print_endline "" ;
	str

let _ = 
  let usage = Printf.sprintf
    "Usage: %s -u <username> -p <password> -n <pr-number> -r <repo> -d <destination-branch> -b <new-branch-name>"
    Sys.argv.(0)
  in
  let username = ref None in
  let password = ref None in
  let issue_number = ref None in
  let repo = ref None in
  let dest_branch = ref None in
  let branch_name = ref None in
  Arg.parse
    [
      ("-u", Arg.String (fun x -> username := Some x), "Github username");
      ("-p", Arg.String (fun x -> password := Some x), "Github password (optional)");
      ("-n", Arg.Int (fun x -> issue_number := Some x), "Github issue number");
      ("-r", Arg.String (fun x -> repo := Some x), "Github repo");
      ("-d", Arg.String (fun x -> dest_branch := Some x), "Github destination branch");
      ("-b", Arg.String (fun x -> branch_name := Some x), "Github new branch name");
    ]
    (fun x -> Printf.eprintf "Warning: ignoring unexpected argument %s\n" x)
    usage;
  match !username, !issue_number, !repo, !dest_branch, !branch_name with
  | Some u, Some n, Some r, Some d, Some b ->
    let pass = match !password with
      | None -> read_line_no_echo ~pre:"Password: " ()
      | Some p -> p in
    Printf.printf "OK.\n";
    Lwt_main.run (
      pr_info ~user:u ~pass ~issue_number:n ~repo:r ~dest_branch:d
        ~branch_name:b
    )
  | _ ->
    print_endline usage;
    exit 1
