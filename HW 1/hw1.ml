open List 
open Pervasives 

(* Returns true if set (a) is a subset of set (b) *)
let rec subset a b = 
	match a with 
	| [] -> true
	| hd::tl -> if not (mem hd b) then false else (subset tl b);;

(* Returns true if sets (a) and (b) are equal sets *)
let equal_sets a b =
	subset a b && subset b a;;

(* Returns union of sets (a) and (b) *)
let rec set_union a b =
	match a with 
	| [] -> b 
	| hd::tl -> if not (mem hd b) then hd::(set_union tl b) else (set_union tl b);;

(* Returns intersection of sets (a) and (b) *)
let rec set_intersection a b = 
	match a with 
	| [] -> []
	| hd::tl -> if (mem hd b) then hd::(set_intersection tl b) else (set_intersection tl b);;

(* Returns diff of sets (a) and (b), that is, all of the elements of (a) that are not present in (b) *)
let rec set_diff a b = 
	match a with 
	| [] -> []
	| hd::tl -> if not (mem hd b) then hd::(set_diff tl b) else (set_diff tl b);;

(* Returns computed fix point given a predicate (eq), function (f), and starting point (x). Infinite loops if fixed point is non-existent *)
let rec computed_fixed_point eq f x = 
	if (eq (f x) x) then x else (computed_fixed_point eq f (f x));;


type ('nonterminal, 'terminal) symbol =
  | N of 'nonterminal
  | T of 'terminal;;

(* Simple filter helper function that removes any symbols that are of terminal symbol type*)
let rec filter_terminals symbols =
	match symbols with 
	| [] -> []
	| N next_symbol::remaining_symbols -> next_symbol::(filter_terminals remaining_symbols)
	| T next_symbol::remaining_symbols -> filter_terminals remaining_symbols;;

(* This function is the dfs function to add to a list of reachable nonterminal symbols, given by the reachable_nonterminals argument. We iterate through 
the list of rules and see if the LHS of the rule is reachable from our reachable_nonterminal list. If so, then we add the nonterminal symbols of that rule's
RHS (after filtering the terminal symbols) to our reachable_nonterminal list and continue iterating through the rules. If the LHS is not reachable at the
moment, we simply continue iterating through the rules without changing the reachable_nonterminals list. Finally, if er have reached the end of the rules,
we return the reachable_nonterminals list! *)
let rec rule_dfs reachable_nonterminals rules = 
	match rules with 
	| [] -> reachable_nonterminals
	| next_rule::remaining_rules -> 
	if mem (fst next_rule) reachable_nonterminals then rule_dfs (set_union (filter_terminals (snd next_rule)) reachable_nonterminals) remaining_rules
	else rule_dfs reachable_nonterminals remaining_rules;;

(* Results from rule_dfs essentially serve as sets. If sets have the same contents, they are equal and so the result is the same as no new nonterminal
symbols were discovered, so we can end the overall recursion. *)
let stagnant_result result_one result_two = 
	equal_sets result_one result_two ;;

(* This function first does a preliminary dfs on the list of rules given a list of reachable nonterminal symbols, which at first is simply a list
containing the start symbol. After this, we do another dfs, as we may have skipped over nonterminals in the rules that we were able to reach due to later
rules in the list. After, we compare the results of both dfs' and if they are the same, we have a final list of non-terminals. If not, then there may be
more reachable nonterminals and we must recurse on the overall function over again to ensure the full list of nonterminals is discovered. *)
let rec find_reachable_nonterminals reachable_nonterminals rules = 
	let first_dfs_result = rule_dfs reachable_nonterminals rules in 
	let second_dfs_result = rule_dfs first_dfs_result rules in
	if stagnant_result first_dfs_result second_dfs_result then first_dfs_result else find_reachable_nonterminals second_dfs_result rules;;

(* First, do a specialized dfs (find_reachable_nonterminals) on the rules and return a list of nonterminal symbols that can be reached. Then with that list, filter the full list of rules by 
removing those NOT in the reachable list. Finally, return the start symbol and the filtered list. *)
let filter_reachable g =
	let start_symbol = fst g in
	let rules = snd g in 
	let reachable_nonterminals = find_reachable_nonterminals [start_symbol] rules in 
	let filtered_rules = filter (fun x -> mem (fst x) reachable_nonterminals) rules in
	(start_symbol, filtered_rules);;
