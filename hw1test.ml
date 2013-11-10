let my_subset_test0 = subset [] []
let my_subset_test1 = not (subset [1,2,3] [])
let my_subset_test2 = subset ["a"] ["a";"b";"c"]
let my_subset_test3 = subset ["uno";"dos";"tres"] ["uno";"dos";"tres";"cuatro"]

let my_equal_sets_test0 = equal_sets [] []
let my_equal_sets_test1 = not (equal_sets ["a";"b"] ["e";"d";"c";"b";"a"])
let my_equal_sets_test2 = not (equal_sets [] ["a"])
let my_equal_sets_test3 = not (equal_sets [1] [])

let my_computed_fixed_point_test0 =
	computed_fixed_point (=) (fun x -> 0) 9001 = 0
let my_computed_fixed_point_test1 = 
	computed_fixed_point (=) (fun x -> x / 2) (-1000000000) = 0

type my_awksub_nonterminals =
  | Uno | Dos | Tres | Cuatro | Cinco

let my_awksub_rules =
   [Uno, [];
    Dos, [T"$"; N Uno];
    Tres, [N Cuatro];
    Cuatro, [N Tres];
    Cinco, [N Uno; N Dos; N Tres; N Cuatro]]

let my_filter_reachable_test0 =
	filter_reachable (Uno, my_awksub_rules) = (Uno, [(Uno, [])]) 
let my_filter_reachable_test1 =
	filter_reachable (Tres, my_awksub_rules) = 
		(Tres, [(Tres, [N Cuatro]); (Cuatro, [N Tres])])
let my_filter_reachable_test2 =
	filter_reachable (Cinco, my_awksub_rules) =
		(Cinco,
		[(Uno, []); (Dos, [T "$"; N Uno]); (Tres, [N Cuatro]); 
	 	(Cuatro, [N Tres]); (Cinco, [N Uno; N Dos; N Tres; 
		N Cuatro])])

