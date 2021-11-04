(* 5 teden moduli funktorji (lahko zaenkrat v globalnem okolju) *)

(********* IZJEME *******)
(* vse izjeme so tipa exn *)

datatype ('prvi, 'drugi) seznamParov = Prazen | Element of 'prvi * 'drugi * ('prvi, 'drugi) seznamParov;  
type 'a multiMnozica = ('a, int) seznamParov;

(* seznamParov: 'a list * 'b list -> ('a,'b) seznamParov   ni tuple!*)

fun seznamParov(x::xs, y::ys) =
    Element (x,y,seznamParov(xs,ys))
    | seznamParov _ = Prazen

(* val map = fn : ('a -> 'b) * 'a list -> 'b list
val filter = fn : ('a -> bool) * 'a list -> 'a list
val foldl = fn : ('a * 'b -> 'a) * 'a * 'b list -> 'a
val foldr = fn : ('a * 'b -> 'a) * 'a * 'b list -> 'a *)

(* funkcija, zacetna vrednost, seznam --> vrne n-krat aplikacijo funkcije na zacetno vrednost (acc) (odvisno od dolzine seznama) *)
fun foldl (_, z, []) = z
    | foldl (f, z, h::t) =  foldl(f,f(z,h),t)

(* obrni seznam in klici foldr*)
(* ListPair.foldr - uporablja currying*)

fun foldr (f,z,s) = foldl(f, z, List.rev s)

(* rezultat rekruzije *)
fun foldr' (_, z, []) = z
    | foldr' (f,z,h::t) = f (foldr(f,z,t), h)

(* repno rekurzivno *)
fun foldr'' (f,z,s) = foldl (f, z, foldl (fn (z,x) => x::z , [], s))

fun map (f,s) = foldr (fn (z,x) => f x :: z, [], s) (* preslikamo element in ga damo na zacetek*)

fun filter(f,s) = foldr (fn (z,x) => if f x then x :: z else z, [], s) (* ce ustreza f x, dodaj x na zacetek seznama, sicer ga spustimo*)

fun append(s1,s2) = foldr (fn(z,x) => x::z , s1, s2) (* x element ki ga dodajamo, dodajamo s konca enega na drugi seznam*)
(* s foldrjem zadnjo vrednost s1 damo na s2 veckrat, nasa funkcija dodaja element na zacetek *)
(* zip seznamParov *)
(* subtract izjeme *)


datatype natural = Succ of natural | One;
exception NotNaturalNumber;

datatype 'a bstree = br of 'a bstree * 'a * 'a bstree | lf;
datatype direction = L | R;

fun zip (x::xs, y::ys) = (x,y)::zip(xs,ys)
   | zip _ = [];

(* nekatero od današnjih funkcij *)
(* case razdeli na dva seznama *)
(* ni funkcije s pari *)
fun unzip ((g1,g2)::r) = map (fn ((g1,g2),r) => (g1,g2), [])
    | unzip _ = [];

(* Succ(Succ(One))
(Succ(One) *)

fun subtract (a,b) = 
    case b of 
        One => (case a of 
                    One => raise NotNaturalNumber   
                    | Succ z => z)
        | Succ x => (case a of 
                        One => raise NotNaturalNumber
                        | Succ y => subtract(y,x))
    

(* any(fn x => x>3 ,[1,2,3,4,5]) *)

fun any (f,s) =
    case s of
        (g::r) => f g orelse any(f, r);
        | _ => false;

fun rotate (bst,d) = 
    if d=L 
    then
        case bst of
            (l,x,r) => (#1 l,#2 l, bst (#3 l, x, #3 x))
            | _ => lf 
    else 
        case bst of
            (l,x,r) => (bst (#1 x, x, #1 r) ,#2 r, #3 r)
            | _ => lf


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
