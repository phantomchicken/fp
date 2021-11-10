datatype 'a bstree = br of 'a bstree * 'a * 'a bstree | lf;
datatype direction = L | R;

fun countLeaves (tree) : int =
    case tree of
        lf => 1
        | br n => countLeaves(#1 n) + countLeaves(#3 n); 

fun countBranches (tree) : int = 
    case tree of
        lf => 0
        | br n => 2 + countBranches(#1 n) + countBranches(#3 n);

fun height (tree) : int = 
    case tree of
        lf => 1
        | br n => if (countLeaves(#1 n) >= countLeaves (#3 n)) then 1 + height(#1 n)
                    else 1 + height(#3 n);

fun avl (f, bst, e) =
    case bst of
        br(l,x,r) => (if f x 
                      then avl (f,l,e)
                      else avl (f,r,e))
        | lf => br(lf,e,lf);

(* izpis daljših izrazov v interpreterju *)
val _ = Control.Print.printDepth := 100;
val _ = Control.Print.printLength := 1000;
val _ = Control.Print.stringDepth := 1000;

(* izpis drevesa po nivojih *)
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

(* primeri vstavljanja elementov v AVL drevo *)
fun avlInt (t, i) = avl (Int.compare, t, i);
fun showTreeInt t = showTree(Int.toString, t);

val tr = lf : int bstree;
val _ = showTreeInt tr;
val tr = avlInt (tr, 1);
val _ = showTreeInt tr;
val tr = avlInt (tr, 2);
val _ = showTreeInt tr;
val tr = avlInt (tr, 3);
val _ = showTreeInt tr;
val tr = avlInt (tr, 4);
val _ = showTreeInt tr;
val tr = avlInt (tr, 5);
val _ = showTreeInt tr;
val tr = avlInt (tr, 6);
val _ = showTreeInt tr;
val tr = avlInt (tr, 7);
val _ = showTreeInt tr;
val tr = avlInt (tr, ~4);
val _ = showTreeInt tr;
val tr = avlInt (tr, ~3);
val _ = showTreeInt tr;
val tr = avlInt (tr, ~2);
val _ = showTreeInt tr;
val tr = avlInt (tr, ~1);
val _ = showTreeInt tr;
val tr = avlInt (tr, 0);
val _ = showTreeInt tr;

val from0to13 = fold (fn (z, x) => avl (Int.compare, z, x), lf, List.tabulate (14, fn i => i));