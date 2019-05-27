type nonterminals = 
	| NT0 | NT1 | NT2

let my_grammar = 
(NT0, function
     | NT0 ->
        [[N NT2; T "Term1"];
         [N NT1; N NT2]]
     | NT1 ->
	 	[[T "Term6"; N NT2];
	 	 [T "Term7"; N NT0]]
     | NT2 ->
	 	[[T "Term2"];
	  	[T "Term3"; T "Term4"; T "Term5"]])

let match_frag = ["Term6"; "Term3"; "Term4"; "Term5"; "Term2"; "Yaes"]

let test_acceptor frag = match frag with 
	| "Yaes"::rest -> Some rest
	| _ -> None

let make_matcher_test = ((make_matcher my_grammar test_acceptor match_frag) = Some [])

let parse_frag = ["Term6"; "Term3"; "Term4"; "Term5"; "Term2"]

let make_parser_test = match (make_parser my_grammar parse_frag) with 
	| Some parse_tree -> parse_tree_leaves parse_tree = parse_frag
	| _ -> false
