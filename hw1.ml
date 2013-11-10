let rec subset s1 s2 = match s1 with
[] -> true
| h::t -> (List.mem h s2) && (subset t s2);;

let equal_sets s1 s2 = (subset s1 s2) && (subset s2 s1);;

let rec computed_fixed_point eq f x = if eq x (f x) then x 
else computed_fixed_point eq f (f x);;

type ('nonterminal, 'terminal) symbol =
| N of 'nonterminal
| T of 'terminal;;

let rec is_nt tup = match tup with
| [] -> []
| N x :: t -> x :: is_nt t
| T _ :: t -> is_nt t;;

let rec is_reachable nt r  = nt :: (match r with
| [] -> []
| h::t -> match h with
 	| a,b -> if a = nt then (is_nt b)@(is_reachable nt t)
		else (is_reachable nt t)
	| _ -> []);;

let rec append ntlist r =
ntlist @ (match ntlist with
| [] -> []
| h::t -> (is_reachable h r) @ (append t r)
);;

let filter endlist r =
List.filter (fun (x, y) -> List.mem x endlist) r;;

let rec filter_reachable g = 
(Pervasives.fst g), (filter 
	(computed_fixed_point 
		equal_sets 
			(fun x -> (append x (Pervasives.snd g))) 
			[Pervasives.fst g]
		) 
(Pervasives.snd g));;
