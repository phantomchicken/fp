fun tl2 x = tl x handle List.Empty => []; 

datatype ('tipPrvega, 'tipDrugega) par = Par of 'tipPrvega * 'tipDrugega;

(* datatype (tipi) ime = konstruktor of x * y ... *)

Par(3,"");

datatype ('prvi, 'drugi) seznamParov = Prazen | Element of 'prvi * 'drugi * ('prvi, 'drugi) seznamParov;  
type 'a multiMnozica = ('a, int) seznamParov;

Element (3, 3, Element (4, 3, Prazen)): int multiMnozica;

(* Sprogramiranj funkcijo seznamParov: 'a list * 'b list -> ('a,'b) seznamParov *)

val it = Element (1,"a",Element (2,"b",Element (3,"c",Prazen))) : (int,string) seznamParov

fun seznamParov (x::xs, y::ys) = Element (x,y,seznamParov(xs,ys))
    | seznamParov _ = Prazen; 

(* anonimne funkcije, slabe ker se ne morejo rekurzivno klicati  *)
(* aplikacija (3,4) *)
(fn (x,y) => x+y) (3,4);


(* ------------------------------------------------ *)
(* foldl (f, z, s) vrne  *)
(* f(... f(f(z, s_1), s_2),... s_n) f(…f(f(z,s1​),s2),…s n ). *)
(* s je seznam *)
(* z je rezultat *)
(* f je funkcija *)

fun foldl (f: 'b * 'a -> 'b, z:'b, s: 'a list) = 
    case s of 
    [] => z     (* ce prazen samo vrni rez*)
    | g::r => foldl(f,f (z,g),r); (* funckija ista, novi seznam je rep, rezultat je rez. funkcije na rezultatu in glavi *)

foldl ((fn (x,y)=> x + y), 0, [1,2,3,4]);

fun foldr (f: 'b * 'a-> 'b , z: 'b ,s: 'a list) =
    case s of
    [] => z
    | g::r => foldr(f, f(g,z), r);