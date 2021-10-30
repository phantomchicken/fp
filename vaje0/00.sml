(* Vrne naslednika števila n. *)
fun next (n : int) : int = n + 1;

(* Vrne vsoto števil a in b. *)
fun add (a : int, b : int) : int = a + b;

(* Vrne true, če sta vsaj dva argumenta true, drugače vrne false *)
fun majority (a : bool, b : bool, c : bool) : bool = if (a andalso b orelse b andalso c orelse a andalso c) then true else false;

(* Vrne mediano argumentov - števila tipa real brez (inf : real), (~inf : real), (nan : real) in (~0.0 : real)
   namig: uporabi Real.max in Real.min 2realmax 2realmin *)

Real.min(Real.max(a,b),b) -> manjsi od a,b
Real.max(Real.min(b,c),c) -> manjsi od b,c

Real.min(Real.min(Real.max(a,b),b),Real.max(b,c))

a,b,c
a,c,b
b,a,c
b,c,a
c,a,b
c,b,a

fun median (a : real, b : real, c : real) : real =
   if (a >=c) then Real.min (a,Real.max (b,c)) 
   else Real.max (a, Real.min (b,c));

(* Preveri ali so argumenti veljavne dolžine stranic nekega trikotnika - trikotnik ni izrojen *)
fun triangle (a : int, b : int, c : int)  : bool = 
   a + b > c andalso b + c > a andalso a + c > b andalso a>0 andalso b>0 andalso c>0;
