datatype number = Zero | Succ of number | Pred of number;

(* Negira število a. Pretvorba v int ni dovoljena! *)
fun neg (a : number) : number = 
    case a of
        Zero => a
        | Succ s => Pred (neg s)
        | Pred p => Succ (neg p) 

fun simp (a: number) : number =
    case a of
        Zero => a
        | Succ a => (case simp a of
                        Pred b => b
                        | b => Succ b)
        | Pred a => (case simp a of
                        Succ b => b
                        | b => Pred b)

(* Vrne vsoto števil a in b. Pretvorba v int ni dovoljena! *)
fun add (a : number, b : number) : number =
    case a of
        Zero => simp b
        | Succ Zero => add (Zero, Succ b)
        | Pred Zero => add (Zero, Pred b)
        | Succ s => add (s, Succ b)
        | Pred p => add (p, Pred b)


(* Vrne rezultat primerjave števil a in b. Pretvorba v int ter uporaba funkcij `add` in `neg` ni dovoljena!
    namig: uporabi funkcijo simp *)
fun comp (a : number, b : number) : order = 
    case a of
        Succ s1 => (case b of
                    Succ s2 => comp(simp s1,simp s2)
                    | Pred p2 => GREATER
                    | Zero => GREATER)
        | Pred p1 => (case b of
                    Succ s3 => LESS
                    | Pred p3 => comp(simp p1,simp p3)
                    | Zero => LESS)
        | Zero => (case b of
                    Zero => EQUAL
                    | Succ s4 => LESS
                    | Pred p4 => GREATER)
 
(* SIMP?? *)

(* Kakšen je tip order? *)

datatype tree = Node of int * tree * tree | Leaf of int;

(* Vrne true, če drevo vsebuje element x. *)
fun contains (tree : tree, x : int) : bool = 
    case tree of
        Leaf l => x=l
        | Node n => #1 n=x orelse contains(#2 n,x) orelse contains(#3 n, x)


(* Vrne število listov v drevesu. *)
fun countLeaves (tree : tree) : int =
    case tree of
        Leaf _ => 1
        | Node n => countLeaves(#2 n) + countLeaves(#3 n) 


(* Vrne število vej v drevesu. *)
fun countBranches (tree : tree) : int = 
    case tree of
        Leaf _ => 0
        | Node n => 2 + countBranches(#2 n) + countBranches(#3 n)

(* Vrne višino drevesa. Višina lista je 1. *)
fun height (tree : tree) : int = 
    case tree of
        Leaf _ => 1
        | Node n => if (countLeaves(#2 n) >= countLeaves (#3 n)) then 1 + height(#2 n)
                    else 1 + height(#3 n)


(* Pretvori drevo v seznam z vmesnim prehodom (in-order traversal). *)

fun toList (tree : tree) : int list =
    case tree of
        Node (s, Leaf l, d) => l :: s :: toList d
        | Node (s, l, Leaf d) => (toList l) @ [s] @ [d]
        | Node (s, l, d) => (toList l) @ [s] @ (toList d)
        | Leaf x => [x]


(* Vrne true, če je drevo uravnoteženo:
 * - Obe poddrevesi sta uravnoteženi.
 * - Višini poddreves se razlikujeta kvečjemu za 1.
 * - Listi so uravnoteženi po definiciji.
 *)
fun isBalanced (tree : tree) : bool =
    case tree of 
        Leaf x => true
        | Node (s, Leaf l, d) => abs (1 - height (d))<=1 
        | Node (s, l, Leaf d) => abs (height (l) - 1)<=1
        | Node (s, l, d) => isBalanced(l) andalso isBalanced(d)

(* Vrne true, če je drevo binarno iskalno drevo:
 * - Vrednosti levega poddrevesa so strogo manjši od vrednosti vozlišča.
 * - Vrednosti desnega poddrevesa so strogo večji od vrednosti vozlišča.
 * - Obe poddrevesi sta binarni iskalni drevesi.
 * - Listi so binarna iskalna drevesa po definiciji.
 *)


fun isBST (tree : tree) : bool = 
    case tree of
        Leaf x => true
        | Node (s, Leaf l, Leaf d) => s>l andalso s<d
        | Node (s, Leaf l, Node d) => s>l andalso s<(#1 d) andalso isBST(Node(#1 d, #2 d, #3 d)) 
        | Node (s, Node l, Leaf d) => s<d andalso s>(#1 l) andalso isBST(Node(#1 l, #2 l, #3 l))
        | Node (s, Node l, Node d) => s>(#1 l) andalso s<(#1 d) andalso isBST(Node(#1 l, #2 l, #3 l)) andalso isBST(Node(#1 d, #2 d, #3 d))  