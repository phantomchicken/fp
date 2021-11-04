(*nekaj malo testov brez avl. Pozor: nisem prepričan v pravilnost, če najdete napake, naj se opozori / popravi...*)

(* Kje so testi tipov? *)

val all_tests : bool list ref = ref [];

(* izpis drevesa po nivojih
fun showTree (toString : 'a -> string, t : 'a bstree) =
let fun strign_of_avltree_level (lvl, t) = case t of  
        lf => if lvl = 0 then "nil" else "   "
    |   br (l, n, r) =>
        let val make_space = String.map (fn _ => #" ")
            val sn = toString n
            val sl = strign_of_avltree_level (lvl, l)
            val sr = strign_of_avltree_level (lvl, r)
        in if height t = lvl
            then make_space sl ^ sn ^ make_space sr
            else sl ^ make_space sn ^ sr
        end
    fun print_levels lvl =
        if lvl >= 0
        then (print (Int.toString lvl ^ ": " ^ strign_of_avltree_level (lvl, t) ^ "\n");
                    print_levels (lvl - 1))
        else ()
  in  print_levels (height t)
end;

fun showTreeInt t = showTree(Int.toString, t);
fun showTreeStr t = showTree(fn x=>x, t); *)


val _ = print "---------- zip ----------\n";
val test1 = zip ([1, 2, 3], ["a", "b", "c", "d"]) = [(1, "a"), (2, "b"), (3, "c")];
val test2 = zip ([], [1,2,3]) = [];
val test3 = zip ([1], [1,2]) = [(1,1)];
val _ = (all_tests := !all_tests @ [test1, test2, test3]);

(* val _ = print "---------- unzip ----------\n";
val test1 = unzip [(1, "a"), (2, "b"), (3, "c")] = ([1, 2, 3], ["a", "b", "c"]);
val test2 = unzip ([]) = ([], []);
val _ = (all_tests := !all_tests @ [test1, test2]); *)

val _ = print "---------- subtract ----------\n";
val one = One;
val two = Succ One;
val three = Succ two;
val four = Succ three;
val test1 = subtract (four, three) = one;
val test2 = subtract (four, one) = three;
val test3 = subtract (three, one) = two;
val test4 = (subtract (three, four) handle NotNaturalNumber => One) = One;
val _ = (all_tests := !all_tests @ [test1, test2, test3, test4]);

val _ = print "---------- any ----------\n";
val test1 = any (fn (a,b) => a>b, [(1,2), (2,2), (3,2)]) = true;
val test2 = any (fn (a,b) => a>b, [(1,2), (2,2), (3,3)]) = false;
val test3 = any (fn (a,b) => a>b, [(1,2)]) = false;
val _ = (all_tests := !all_tests @ [test1, test2, test3]);

val _ = print "---------- map ----------\n";
val test1 = map (fn a => a+1, [1,2,3]) = [2,3,4];
val test2 = map (fn a => a+1, []) = [];
val _ = (all_tests := !all_tests @ [test1, test2]);

val _ = print "---------- filter ----------\n";
val test1 = filter (fn a => a>5, [1,2,3]) = [];
val test2 = filter (fn a => a>5, [1,2,3,4,5,6,7]) = [6,7];
val _ = (all_tests := !all_tests @ [test1, test2]);

val _ = print "---------- fold ----------\n";
fun f (a, b) = a+b;
val test1 = fold (f, 0, [1,1,1]) = 3;
val test2 = fold (f, 0, [1,0,0]) = 1;
val test3 = fold (f, 0, [0,0,0]) = 0;
val test4 = fold (f, 1, [0,0,0]) = 1;
val test5 = fold (f, 1, [1,0,0]) = 2;
val _ = (all_tests := !all_tests @ [test1, test2, test3, test4, test5]);

val _ = print "---------- rotate ----------\n";
val treeA = br (lf, 1, lf);
val treeGama = lf;
val treeB = br (treeA, 3, lf);
val tmp = br (lf, 3, lf);
val rotatedB = br (lf, 1, tmp);
val test1 = rotate (treeB, R) = rotatedB;
val test2 = rotate (rotatedB, L) = treeB;
val _ = (all_tests := !all_tests @ [test1, test2]);

val _ = print "---------- height ----------\n";
val treeA = br (lf, 1, lf);
val treeGama = lf;
val treeB = br (treeA, 3, lf);
val tmp = br (lf, 3, lf);
val rotatedB = br (lf, 1, tmp);
val test1 = height (treeA) = 2;
val test2 = height (lf) = 1;
val test3 = height (treeB) = 3;
val _ = (all_tests := !all_tests @ [test1, test2, test3]);

val _ = print "---------- balance ----------\n";
val treeA = br (lf, 1, lf);
val treeGama = lf;
val treeB = br (treeA, 3, lf);
val tmp = br (lf, 3, lf);
val rotatedB = br (lf, 1, tmp);
val test1 = balance treeB = ~1;
val test2 = balance rotatedB = 1;
val test3 = balance treeA = 0;
val t1 = br (lf, "t1", lf);
val t2 = br (lf, "t2", lf);
val t3 = br (lf, "t3", lf);
val t4 = br (lf, "t4", lf);
val x = br (t3, "x", t4);
val y = br (t2, "y", x);
val z = br (t1, "z", y);
val test4 = balance z = 2;
val _ = (all_tests := !all_tests @ [test1, test2, test3, test4]);

val _ = print "---------- rebalance ----------\n";
val t1 = lf;
val t2 = lf;
val t3 = lf;
val t4 = lf;
val x = br (t3, "x", t4);
val y = br (t2, "y", x);
val z = br (t1, "z", y);
val rebalancedZ = br (br (t1, "z", t2), "y", br (t3, "x", t4));
val test1 = rebalance z = rebalancedZ;
val x1 = br (t2, "x", t3);
val y1 = br (x1, "y", t4);
val z1 = br (t1, "z", y1);
val rebalancedZ1 = br (br (t1, "z", t2), "x", br (t3, "y", t4));
val test2 = rebalance z1 = rebalancedZ1;
val x2 = br (t4, "x", t3);
val y2 = br (x2, "y", t2);
val z2 = br (y2, "z", t1);
val rebalancedZ2 = br (x, "y", br (t2, "z", t1));
val test3 = rebalance z2 = rebalancedZ2;
val x3 = br (t3, "x", t2);
val y3 = br (t4, "y", x3);
val z3 = br (y3, "z", t1);
val rebalancedZ3 = br (br (t4, "y", t3), "x", br (t2, "z", t1))
val test4 = rebalance z3 = rebalancedZ3;
val _ = (all_tests := !all_tests @ [test1, test2, test3, test4]);

(* aggregation of all restuts *)
val nr_passes_tests = foldl (fn (true, acc) => acc + 1 | (false, acc) => acc) 0 (!all_tests);
val nr_all_tests = length (!all_tests);