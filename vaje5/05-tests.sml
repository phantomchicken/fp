(*manjkajo testi za REDUCE - vgnezdeni seznami se mi ne prekopirajo pravilno...*)

val all_tests : bool list ref = ref [];

val _ = print "---------- squares ----------\n";
val _ : int list -> int list = squares;
val test1 = squares [] = [];
val test2 = squares [1,2,3] = [1,4,9];
val test3 = squares [3,~3,~1] = [9,9,1];
val _ = (all_tests := !all_tests @ [test1, test2, test3]);

val _ = print "---------- onlyEven ----------\n";
val _ : int list -> int list = onlyEven;
val test1 = onlyEven [] = [];
val test2 = onlyEven [1,2,3] = [2];
val test3 = onlyEven [3,~3,~1] = [];
val test4 = onlyEven [1,2,3,4,5,6] = [2,4,6];
val _ = (all_tests := !all_tests @ [test1, test2, test3, test4]);

val _ = print "---------- bestString ----------\n";
val _ : (string * string -> bool) -> string list -> string = bestString;
val test1 = bestString (fn (x,y) => size x > size y) [] = "";
val test2 = bestString (fn (x,y) => size x > size y) ["a", "bb", "c"] = "bb";
val test3 = bestString (fn (x,y) => size x > size y) ["a", "bb", "ccc"] = "ccc";
val test4 = bestString (fn (x,y) => size x > size y) ["lalalalla", "LLLLLLLLLLLLLLLLLLL"] = "LLLLLLLLLLLLLLLLLLL";
val _ = (all_tests := !all_tests @ [test1, test2, test3, test4]);

val _ = print "---------- largestString ----------\n";
val _ : string list -> string = largestString;
val test1 = largestString [] = "";
val test2 = largestString ["", "aaaaaaa", "vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv", "a"] = "vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv";
val test3 = largestString ["b", "a", "c"] = "c";
val test4 = largestString ["arra", "raarara", "aaa"] = "raarara";
val _ = (all_tests := !all_tests @ [test1, test2, test3, test4]);

val _ = print "---------- longestString ----------\n";
val _ : string list -> string = longestString;
val test1 = longestString [] = "";
val test2 = longestString ["a", "bb", "c"] = "bb";
val test3 = longestString ["a", "bb", "ccc"] = "ccc";
val test4 = longestString ["lalalalla", "LLLLLLLLLLLLLLLLLLL"] = "LLLLLLLLLLLLLLLLLLL";
val _ = (all_tests := !all_tests @ [test1, test2, test3, test4]);

val _ = print "---------- quicksort ----------\n";
val _ : ('a * 'a -> order) -> 'a list -> 'a list = quicksort;
val test1 = quicksort (fn (x,y) => if size x > size y then GREATER else if size x = size y then EQUAL else LESS) ["a", "bbbb", "ccc"] = ["a", "ccc", "bbbb"];
val test2 = quicksort (fn x => String.compare x) ["bbbb", "ccc", "a"] = ["a", "bbbb", "ccc"];
val test3 = quicksort (fn (x, y) => if x >y then GREATER else if x = y then EQUAL else LESS ) [4,1,3,2,1,2,3] = [1,1,2,2,3,3,4];
val _ = (all_tests := !all_tests @ [test1, test2, test3]);

val _ = print "---------- dot ----------\n";
val _ : int list -> int list -> int = dot;
val test1 = dot [0,0,0] [0,0,0] = 0;
val test2 = dot [1,0,0] [2,0,0] = 2;
val test3 = dot [2,1,2] [1,1,2] = 7;
val _ = (all_tests := !all_tests @ [test1, test2, test3]);

val _ = print "---------- transpose ----------\n";
val _ : 'a list list -> 'a list list = transpose;
val test1 = transpose [ [1,2], [3,4] ] = [ [1, 3], [2,4] ];
val test2 = transpose [ [1,2,3],[4,5,6],[7,8,9] ] = [ [1,4,7], [2,5,8], [3,6,9] ];
val test3 = transpose [ [1] ] = [ [1] ];
val _ = (all_tests := !all_tests @ [test1, test2, test3]);

val _ = print "---------- multiply ----------\n";
val _ : int list list -> int list list -> int list list = multiply;
val test1 = multiply [ [1,2], [3,4] ] [ [1,2], [3,4] ] = [ [7, 10], [15,22] ];
val test2 = multiply [ [1,2,3],[4,5,6],[1,1,1] ] [ [1,2,3],[4,5,6],[1,1,1] ] = [ [12,15,18], [30,39,48], [6,8,10] ];
val test3 = multiply [ [1] ] [ [1] ] = [ [1] ];
val _ = (all_tests := !all_tests @ [test1, test2, test3]);

val _ = print "---------- group ----------\n";
val _ : ''a list -> (''a * int) list = group;
val test1 = group [1,2,2,3,3,3,4,4,4,4] = [(1,1), (2,2), (3,3), (4,4)];
val test2 = group [1,2,3,4] = [(1,1), (2,1), (3,1), (4,1)];
val test3 = group [1,1,1,2,1,1] = [(1,3), (2,1), (1,2)];
val _ = (all_tests := !all_tests @ [test1, test2, test3]);

val _ = print "---------- equivalenceClasses ----------\n";
val _ : ('a -> 'a -> bool) -> 'a list -> 'a list list = equivalenceClasses;
val test1 = equivalenceClasses (fn x => fn y => x=y) [1,2,2,3,3,3,4,4,4,4] = [ [1], [2,2], [3,3,3], [4,4,4,4] ];
val test2 = equivalenceClasses (fn x => fn y => size x = size y) ["aaa","c", "bbb", "ala", "a", "v", "123"] = [ ["aaa", "bbb", "ala", "123"], ["c", "a", "v"] ];
val test3 = equivalenceClasses (fn x => fn y => size x = size y) ["a", "aa", "aaa"] = [ ["a"], ["aa"], ["aaa"] ];
val _ = (all_tests := !all_tests @ [test1, test2, test3]);

(* aggregation of all restuts *)
val nr_passes_tests = foldl (fn (true, acc) => acc + 1 | (false, acc) => acc) 0 (!all_tests);
val nr_all_tests = length (!all_tests);