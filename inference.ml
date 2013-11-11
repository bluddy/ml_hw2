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
  mutable debug_send: bool;
  mutable print_tree: bool;
}

let params = {
  action=Inference;
  network_file="";
  cpd_file="";
  cliquetree_file="";
  queries_file="";
  debug_send=false;
  print_tree=false;
}

let parse_cmd_line () =
  let files = ref [] in
  let param_specs = Arg.align
   ["--print_cpds", Arg.Unit (fun () -> params.action <- Print_CPDs),
     "Only print the CPDs";
    "--print_init_tree", Arg.Unit (fun () -> params.action <- Print_Tree),
     "Only print the initialized clique tree";
    "--print_tree", Arg.Unit (fun () -> params.print_tree <- true),
     "Print the final clique tree";
    "--debug_send", Arg.Unit (fun () -> params.debug_send <- true),
     "Debug: print send_msg information";
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

let print_tree tree = print_endline @: string_of_tree tree

let run () = 
  let tree = parse_clique_tree params.cliquetree_file in
  (*print_endline "parsed clique tree";*)
  set_tree_sepsets tree;
  (*print_endline "set sepsets";*)
  let cpd_list = parse_cpd params.cpd_file in
  (*print_endline "parsed cpds";*)
  let tree = tree_fill_cpds tree cpd_list in
  (*print_endline "filled tree with cpds";*)
  match params.action with
  | Print_CPDs -> print_endline @: string_of_cpd_list cpd_list
  | Print_Tree -> print_tree tree
  | Inference  -> 
      upstream tree ~print_send:params.debug_send;
      if params.debug_send then print_endline "Downstream...";
      downstream tree ~print_send:params.debug_send;
      if params.debug_send then print_endline "Tree:";
      if params.print_tree then print_tree tree else ()

let _ = 
  if !Sys.interactive then ()
  else
    (parse_cmd_line ();
    run ();)


