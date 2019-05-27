type ('nonterminal, 'terminal) symbol = | N of 'nonterminal | T of 'terminal;;

type ('nonterminal, 'terminal) parse_tree =
  | Node of 'nonterminal * ('nonterminal, 'terminal) parse_tree list
  | Leaf of 'terminal;;

(* Convert Grammar *)

let rec build_production_function nonterminal = function 
	| [] -> []
	| (lhs, rhs)::rest -> let result_of_rest = build_production_function nonterminal rest in
	if lhs = nonterminal then rhs::result_of_rest else result_of_rest;;

let convert_grammar gram1 = match gram1 with
	(start_symbol, rules) -> (start_symbol, fun nonterminal -> build_production_function nonterminal rules);;


(* Parse Tree Leaves *)

let rec recurse_on_parse_tree_list = function 
	| [] -> []
	| curr::rest -> let result_of_rest = recurse_on_parse_tree_list rest in
	(recurse_on_next_tree curr) @ (result_of_rest)
and recurse_on_next_tree = function 
	| Leaf x -> [x]
	| Node (nonterminal, parse_tree_list) -> recurse_on_parse_tree_list parse_tree_list;;

let parse_tree_leaves tree = recurse_on_next_tree tree;;


(* Make Matcher *)

let make_matcher gram accept frag =  

	let start_symbol = fst gram in 	
	let production_function = snd gram in 
	let start_rule_list = production_function start_symbol in

	let rec attempt_rule production_function rule_list accept frag = (match rule_list with
		| [] -> None 	(* No more possible rules to match *)
		| current_rule::rest -> match (match_rule production_function current_rule accept frag) with 
			| None -> attempt_rule production_function rest accept frag (* No match or acceptor fails, try next rule *)
			| Some value -> Some value)									(* We got some value from the acceptor, return it *)
	and match_rule production_function rule accept frag = (match rule with
		| [] -> accept frag (* Rule is complete! Apply the acceptor *)
		| (T current_term)::rest_of_terms -> (match frag with 
			| [] -> None 	(* Rule is not empty, but fragment is exhausted *)
			| current_symbol::rest_of_symbols -> (* Match current symbol and recurse if match succeeds *)
				if current_symbol = current_term then (match_rule production_function rest_of_terms accept rest_of_symbols) else None)	
		| (N current_term)::rest_of_terms -> (match frag with 
			| [] -> None  	(* Rule is not empty, but fragment is exhausted *)
			| frag ->		(* Recurse on the nonterminal to continue matching, but pass in an artificial curried acceptor function that continues matching after *)
				let continuation_curried_acceptor = (match_rule production_function rest_of_terms accept) in 
				(attempt_rule production_function (production_function current_term) continuation_curried_acceptor frag)))
	in

	attempt_rule production_function start_rule_list accept frag;;

(* Make Parser *)

let make_parser gram frag = 

	let start_symbol = fst gram in 	
	let production_function = snd gram in 
	let start_rule_list = production_function start_symbol in

	(* My own acceptor that only accepts an empty fragment and returns the derived parse_tree *)
	let accept_empty_list frag parse_tree = match frag with 
	| [] -> Some parse_tree
	| _ -> None in 

	let rec attempt_rule_parser production_function rule_list accept outer_tree frag parse_tree = (match rule_list with
		| [] -> None 	(* No more rules to try *)
		| current_rule::rest -> match (match_rule_parser production_function current_rule accept outer_tree frag parse_tree) with 
			| None -> attempt_rule_parser production_function rest accept outer_tree frag parse_tree (* Match failure or acceptor failure, try the next rule *)
			| Some accepted_value -> Some accepted_value)									
	and match_rule_parser production_function rule accept outer_tree frag parse_tree = (match rule with
		| [] -> (match outer_tree with (* Must see if an outer_tree was passed to call attempt_rule_parser properly to avoid skipping possible rules if things fail later on, and also append the matched nonterminal to the outer tree *)
			| None -> (accept frag parse_tree) 	(* No outer tree, just call the acceptor *)
			| Some Node (nonterminal, parse_tree_list) -> (accept frag (Node (nonterminal, parse_tree_list @ [parse_tree])))	(* Append to the parse tree, and then call the curried artificial acceptor *)
			| Some Leaf x -> None) (* Never going to happen, but added case to get rid of warning *)
		| (T current_term)::rest_of_terms -> (match frag with 
			| [] -> None (* Nothing left in frag but rule is not fully matched *)
			| current_symbol::rest_of_symbols -> 
				if current_symbol <> current_term then None else match parse_tree with 
				| Node (nonterminal, parse_tree_list) -> (match_rule_parser production_function rest_of_terms accept outer_tree rest_of_symbols (Node (nonterminal, (parse_tree_list @ [(Leaf current_term)]))))
				(* If current symbol matches, continue matching but append to the end of the current parse tree list *)	
				| Leaf x -> None) (* Never going to happen, but added case to get rid of warning *)
		| (N current_term)::rest_of_terms -> (match frag with 
			| [] -> None  	(* Nothing left in frag but rule is not fully matched *)
			| frag -> let curried_artificial_acceptor = match_rule_parser production_function rest_of_terms accept outer_tree in	(* Curried acceptor that continues the matching after matching current nonterminal*)
				(attempt_rule_parser production_function (production_function current_term) curried_artificial_acceptor (Some parse_tree) frag (Node (current_term, [])))))
				(* Attempt to match the next non-terminal, but pass in an outer tree now to keep place in current parse tree list, but also append to the new nonterminal's parse tree in the function call *)
	in

	attempt_rule_parser production_function start_rule_list accept_empty_list None frag (Node (start_symbol, []));;
