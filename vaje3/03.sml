datatype ('prvi, 'drugi) seznamParov = Prazen | Element of 'prvi * 'drugi * ('prvi, 'drugi) seznamParov;  
type 'a multiMnozica = ('a, int) seznamParov;

(* seznamParov: 'a list * 'b list -> ('a,'b) seznamParov   ni tuple!*)

fun seznamParov(x::xs, y::ys) =
    Element (x,y,seznamParov(xs,ys))
    | seznamParov _ = Prazen

(* funkcija, zacetna vrednost, seznam --> vrne n-krat aplikacijo funkcije na zacetno vrednost (acc) (odvisno od dolzine seznama) *)
fun fold (_, z, []) = z
    | fold (f, z, h::t) =  fold(f,f(z,h),t)

(* obrni seznam in klici foldr*)
(* ListPair.foldr - uporablja currying*)

fun foldr (f,z,s) = fold(f, z, List.rev s)

(* rezultat rekruzije *)
fun foldr' (_, z, []) = z
    | foldr' (f,z,h::t) = f (foldr(f,z,t), h)

(* repno rekurzivno *)
fun foldr'' (f,z,s) = fold (f, z, fold (fn (z,x) => x::z , [], s))

fun map (f,s) = foldr (fn (z,x) => f x :: z, [], s) (* preslikamo element in ga damo na zacetek*)

fun filter(f,s) = foldr (fn (z,x) => if f x then x :: z else z, [], s) (* ce ustreza f x, dodaj x na zacetek seznama, sicer ga spustimo*)

fun append(s1,s2) = foldr (fn(z,x) => x::z , s1, s2) (* x element ki ga dodajamo, dodajamo s konca enega na drugi seznam*)
(* s foldrjem zadnjo vrednost s1 damo na s2 veckrat, nasa funkcija dodaja element na zacetek *)
(* zip seznamParov *)
(* subtract izjeme *)

fun zip (x::xs, y::ys) = (x,y)::zip(xs,ys)
   | zip _ = [];

(* nekatero od današnjih funkcij *)
(* case razdeli na dva seznama *)
(* ni funkcije s pari *)

(* unzip [(1, "a"), (2, "b"), (3, "c")] = ([1, 2, 3], ["a", "b", "c"]); *)

(* fun unzip ((g1,g2)::r) = map (fn ((g1,g2)::r) => ((g1::nil),(g2::nil)), [])
    | unzip _ = []; *)

(* fun unzip ((x1,x2)::xs) =
    let
      val (l1,l2) = unzip xs
    in
      (x1::l1, x2::l2)
    end
    | unzip _ = []; *)

datatype natural = Succ of natural | One;
exception NotNaturalNumber;

fun subtract (a,b) = 
    case b of 
        One => (case a of 
                    One => raise NotNaturalNumber   
                    | Succ z => z)
        | Succ x => (case a of 
                        One => raise NotNaturalNumber
                        | Succ y => subtract(y,x))
    
fun any (f,s) =
    case s of
        (g::r) => f g orelse any(f, r)
        | _ => false;


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



fun rotate (bst,d) = 
    if d=L 
    then
        case bst of 
            br(l, x, br(rl, r, rr)) => br(br(l, x, rl), r, rr)
            | _ => bst
    else 
        case bst of 
            br(br(ll, l, lr), x, r) => br(ll, l, br(lr, x, r))
            | _ => bst;


fun balance (bst) =
    case bst of
    br(l,x,r) => height r - height l
    | lf => 0;

fun rebalance(leaf as lf) = lf |
    rebalance (bst as br(l,x,r)) =
    case (balance bst, balance l, balance r) of 
     ( 2, _, ~1) => rotate (br(l, x, rotate (r,R)),L) 
     | ( 2, _, _)  => rotate(bst,L)
     | (~2, 1, _)  => rotate(br(rotate (l,L), x , r),R)
     | (~2, _, _)  => rotate(bst,R)
     | _ => bst


fun avl (f, bst, e) =
    case bst of
        br(l,x,r) => (if f x 
                      then avl (f,l,e)
                      else avl (f,r,e))
        | lf => br(lf,e,lf)


    


(* fun rotate ((l,x, r), L) = br (br(l, x, #1 l), #2 r, #3 r)
    | rotate ((l,x,r), R) = br (#1 l, #2 l, br(#1 r, x, r))
    | rotate (_,_) = lf *)

(* val zip = fn : 'a list * 'b list -> ('a * 'b) list
val unzip = fn : ('a * 'b) list -> 'a list * 'b list
val subtract = fn : natural * natural -> natural
val any = fn : ('a -> bool) * 'a list -> bool
val map = fn : ('a -> 'b) * 'a list -> 'b list
val filter = fn : ('a -> bool) * 'a list -> 'a list
val fold = fn : ('a * 'b -> 'a) * 'a * 'b list -> 'a
val rotate = fn : 'a bstree * direction -> 'a bstree
val rebalance = fn : 'a bstree -> 'a bstree
val avl = fn : ('a * 'a -> order) * 'a bstree * 'a -> 'a bstree *)
