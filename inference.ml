open Util
open CliqueTree
open Cpd

type actions = Print_Tree
             | Print_CPDs
             | Inference

type params_t = {
  mutable action: actions;
  mutable network_file: string;
  mutable cpd_file: string;
  mutable cliquetree_file: string;
  mutable queries_file: string;
}

let params = {
  action=Inference;
  network_file="";
  cpd_file="";
  cliquetree_file="";
  queries_file="";
}

let parse_cmd_line () =
  let files = ref [] in
  let param_specs = Arg.align
   ["--print_cpds", Arg.Unit (fun () -> params.action <- Print_CPDs),
     "Print the CPDs";
    "--print_tree", Arg.Unit (fun () -> params.action <- Print_Tree),
     "Print the clique tree";
   ]
  in
  Arg.parse param_specs
    (fun f -> files := !files @ [f]) (* add files *)
    "network_file cpd_file cliquetree_file queries_file";
  match !files with 
  | [nf;cpd;ct;q] -> params.network_file    <- nf;
                     params.cpd_file        <- cpd;
                     params.cliquetree_file <- ct;
                     params.queries_file    <- q
  | _ -> print_endline 
    "Please specify network_file cpd_file cliquetree_file queries_file";
    exit 1

let run () = 
  let tree = parse_clique_tree params.cliquetree_file in
  let cpd_list = parse_cpd params.cpd_file in
  match params.action with
  | Print_CPDs -> print_endline @: string_of_cpd_list cpd_list
  | Print_Tree -> print_endline @: string_of_tree tree
  | Inference  -> ignore @: tree_fill_cpds tree cpd_list

let _ = 
  if !Sys.interactive then ()
  else
    (parse_cmd_line ();
    run ();)


