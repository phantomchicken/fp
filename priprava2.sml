(* 2019/2020 1.rok *)
(* fun x {a=b, c=d} h = case (b,d) of (SOME e, f::g) => e andalso f andalso (x {a=b, c=g} h ) | (NONE, f::g) => f andalso (x {a=b, c=g} h) | _ => h *)

(* - find 3 [1,3,3,6,3,4,2,3,54,3]; 
val it = [2,3,5,8,10] : nt list *)
(* foldl f acc list *)
(* foldr (fn(x,acc)=>(f (x))::acc ) [] sez *)
fun printList xs = print(String.concatWith ", " (map Int.toString xs) ^ "\n");
fun wrap acc = let val x = printList acc; in acc end
fun find el sez = List.foldl (fn(x,acc)=> if x=el then (List.length acc)::acc else wrap acc) [] sez

(* 2018/2019 1.rok *)
fun poz flist arg limit =
        foldl (fn(x,acc)=> if (x arg)>=limit then acc+1 else acc ) 0 flist

(* 2017/2018 1.rok *)
(* V programskem jeziku SML zapiši funkcijo, ki prejme seznam celih števil. 
Funkcija naj nato izračuna produkt zaporednih elementov v seznamu, torej produkt prvega in drugega 
elementa, produkt drugega in tretjega elementa itn. Rezultat funkcije naj bo torej seznam 
produktov (celih števil), ki je za 1 element krajši od vhodnega seznama. *)
fun f sez = 
        let 
            val tmp = 1::sez
        in tl (ListPair.map (fn(x,y) => x*y) (sez,tmp))
        end

fun a b c = case b of {d=u, e=v} => if (u=1) andalso (v=false) then 3.14 else c b

datatype 'a inner = A of {a:'a} | B of 'a ref
val x1 = [SOME (A {a=3}), SOME (B (ref 5)), SOME (B (ref 5)), NONE, SOME (A {a=3}) ]
val x2 = [(A {a=3}), B (ref 5), B (ref 5), (A {a=3}) ]


fun summarize2 x = 
    let
        fun helper (x,a_rez, b_rez) =
            case x of 
                [] => {a=a_rez, b=b_rez}
                | g::r => case g of  SOME (A {a=a_vr}) => helper (r, a_rez+a_vr, b_rez)
                                |  SOME (B b) => helper (r, a_rez, (b_rez+ !b))
                                | _ => {a=a_rez, b=b_rez}
    in helper (x, 0, 0)
    end

fun summarize x = 
    let
        fun helper (x,a_rez, b_rez) =
            case x of 
                [] => {a=a_rez, b=b_rez}
                | g::r => case g of  (A {a=a_vr}) => helper (r, a_rez+a_vr, b_rez)
                                |  (B b) => helper (r, a_rez, (b_rez+ !b))
    in helper (x, 0, 0)
    end

(* case x of kontruktor {a=a_vr}*)
        
(* 2014/2015 1.rok *)
fun skip seznam = 
    case seznam of 
        [] => [] 
        | [a] => [] 
        | _::drugi::rep => drugi::(skip rep)

fun sum2 seznam = 
    case seznam of 
        [] => [] 
        | [a] => [a] 
        | prvi::drugi::rep => (prvi+drugi)::(sum2 (drugi::rep))   

fun splosna flag seznam =
    case seznam of 
        [] => [] 
        | [a] => if flag then [] else [a]
        | prvi::drugi::rep => if flag then drugi::(splosna flag rep) else (prvi+drugi)::(splosna flag (drugi::rep))

val skip_new = splosna true
val sum2_new = splosna false

(*2014/2015 2.rok*)
fun deli(p, []) = ([], [])
| deli(p, x :: xs) =
    let
        val (b, a) = deli(p, xs)
    in
        if x < p
        then (x :: b, a)
        else (b, x :: a)
    end;

deli( 10, [7, 12, ~3, 88]); (*val it = ([7,~3],[12,88]) : int list * int list*)
(* tip int * int list -> int list * int list *)

val v = [46.0, 69.0, 32.0, 60.0, 52.0, 41.0]
(*5.naloga*)
fun s2 v =
    let
        val n = Real.fromInt(List.length v);
        val kv = List.map (fn(x)=>x*x) v;
        val kv_povp = List.map (fn(x)=>(x*x) / n) v;
    in 
        (foldl (fn(x,acc)=> x - x / n) 0.0 kv) / n-1.0
    end

(*2014 1.rok*)
(*6*)
val seznam = [1,~1, 2, ~2, 3, ~3]
fun prvihNPozitivnih stevila n =
    if n=0 then 0 else 
    case stevila of
        [] => 0
        | g::r => if g > 0 then g + (prvihNPozitivnih r (n-1)) else (prvihNPozitivnih r n)    
    (* List.foldl (fn(x,acc)=>x+acc ) 0 (List.take ((List.filter (fn(x)=>x>0) seznam),n)) *)

(*3*)
datatype 'a Tip1 = A of 'a | B of int
(* 'a * 'b Tip1 -> 'a *)
fun test (x, B y) = x

(*3*)

(* fun sestej a b =
    if a<0 then NONE else SOME ((foldl (fn(x,acc) => x+acc) 0 b) + a) *)

fun sestej a b =
    case (a,b) of
        (_,g::r) => if a<0 then NONE else SOME (g + valOf(sestej a r))
        | (_,_) => SOME a

val sestej3 = (fn (a) => sestej a [3,3,3]);

(*2019/2020 2.rok
2.*)

fun potenca_repna (x,y) = 
    let 
        fun pomozna (x,y,acc) = 
            if y=0 
            then acc 
            else pomozna(x, y-1, acc*x) 
    in pomozna(x,y,1) 
    end 
    
fun vsota_repna sez = 
    let 
        fun pomozna (sez,acc) = 
            if null sez 
            then acc 
            else pomozna(tl sez, acc+(hd sez)) 
    in pomozna(sez,0) 
    end

(* fun repna_obdelava x =
    let
        fun pomozna (arg,acc) = 
            case arg of
                g::r => pomozna (tl arg, acc + g)
                | [] => acc
                | (x,y) => pomozna ((x, y-1), acc*x)
                | _ => 0
        
    in case x of
        g::r => pomozna (x, 0)
        | [] => pomozna (x, 0)
        | (a,b) => pomozna (x, 1)
        | _ => 0
    end *)

(* fun test7 g::r = 1
| test7 (x,y) = 2
| test7 _ = 2 *)

fun fun1 sez: ('a * 'a) list = 
    case sez of 
        g1::g2::r => g2::g1::(fun1 r) 
        | _ => [] 

fun fun2 sez = 
    case sez of 
        (g1,g2)::r => (g2,g1)::(fun2 r) 
        | _ => []

fun splosna f funkcija sez =
    case sez of
        g::r => f (g,r, funkcija)
        | _ => []

fun f1' sez = splosna (fn(g1,r,f) => case r of g2:: rep => g2::g1::(f rep) | _ => []) f1' sez;
fun f2' sez = splosna (fn(g,r,f) => case g of (g1,g2) => (g2,g1) :: (f (tl sez))) f2' sez
(* 
class Jabolko: 
    def teza(self): 
        return 100 
    def kalorije(self): 
        return 80

class Hruska: 
    def teza(self): 
        return 150 
    def kalorije(self): 
        return 120
*)

structure Jabolko =
struct
    fun teza () = 100
    fun kalorije () = 80
end

structure Hruska =
struct
    fun teza () = 150
    fun kalorije () = 120
end