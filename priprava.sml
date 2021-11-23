(*1.naloga 2019/2020*)
fun f1 (a,b,c::d) [i,j] = 
        if c 
        then fn a => b (SOME i) 
        else fn b => a (j+1)

(*1.naloga 2018/2019*)
fun f ({g=g,h=h}, [i,j]) k = 
        if valOf i 
        then fn x => g (k+x) 
        else fn x => h (k-x) ^ "nil"

(*1.naloga 2017/2018*)
fun nekaj a b c d e f = a (b c) (d e);

(*1.naloga 2016/2017*)
fun prva (a, b) c = a::c
fun druga (a, b) c = {a=b andalso c, b=c, c=a}
fun tretja (a, b, c) = ([c a, c b], c) 
fun cetrta [a, b, c] = 
        let  
            val a = 3
            val b = 4 
        in c a = c b 
        end

(*5.naloga 2014/2015*)
fun f1' x y = [x,y]
fun f2' x y = (x,y)
fun f3' x y = {x=x, y=y}

(*
Katere od naslednjih vezav ne uspejo? Izberi enega ali več odgovorov in pri vsakem pojasni, zakaj:
a) val x = f1 1
    b) val x = f2 1
    c) val x = f3 1
d) val x = f1 1 "a"
e) val x = f2 1 "a"
f) val x = f3 1 "a"
*)

(*1.naloga IZP 2019/2020*)
fun x {a=b, c=d} h = 
        case (b,d) of 
            (SOME e, f::g) => e andalso f andalso (x {a=b, c=g} h ) 
        | (NONE, f::g) => f andalso (x {a=b, c=g} h) 
        | _ => h

(* LEKSIKALNI, DINAMICNI OBSEG*)
(* val u = 1
fun f v =
    let
        val u = v + 1
    in
        fn w => u + v + w
    end
val u = 3
val g = f 4
val v = 5
val w = g 6;     *)
val u = 1
fun f v =
        let
            val u = v + 1
        in
            u + v
        end
val g = f 4;
val u = 3;
val g = f 4;


(*1.naloga b KOL 2019/2020*)
val u = 1;
fun f v = 
        fn w => u + v + w
val rez1 = (f 5) 6
val u = 3 
val rez2 = (f 5) 6;

(*1.naloga KOL 2016/2017*)
fun f (a,b) (c::d) e = 
        if (!b) = (c mod 2=0) then 
            fn x => e 
        else fn (y,z) => valOf(z) andalso a mod 2=1;

(*1.naloga KOL 2013/2014*)
fun f (a,b) c =
        let val (b1,b2) = b
        in if a > 3 andalso b2
            then c b1
            else c b2
        end;

(*2.naloga KOL 2013/2014*)
val x = 3
val w = fn _ => x
fun q x =
        let val x = 4
        in w
        end
val rez1 = q 30 15
fun w x = x + 1
val rez2 = q 31 14;

(*4.naloga KOL 2013/2014*)
fun f1 x y z = (x andalso y, z);
fun f2 x y z = (x andalso z, y);
fun f3 x y z = (y andalso z, x);

(*2.naloga IZP2 2018/2019*)
fun F 0 b = 0 | F a b = b + F (a-1) b;
fun Fr 0 b acc = acc | Fr a b acc = Fr (a-1) b acc+b;
fun G a 0 = 1 | G a b = Fr a (G a (b-1)) 0;
(* repno rek, vzajemno rek, pretvori v repno rek, FF 4 5, G 2 10, kaj delata F in G? *)

(*1.naloga b IZP1 2017/2018*)
datatype ('a, 'b) set = elem of 'a list * ('a, 'b) set | empty of 'b 
fun fun1 (x, y) [z] = 
        case y of 
            elem ([nil], empty z) => x 
        | _ => SOME z

(*1.naloga IZP3 2016/2017*)
datatype 'a set = elem of {a:'a, b:'a set} | empty fun fun1 x y = case y of SOME(z) => elem {a=z, b=empty} | NONE => empty

(*1.naloga b IZP2 2015/2016*)
fun mf x y =
        case x y of
            NONE => (fn y => [])

(*1.naloga b IZP3 2015/2016*)
fun mf x y =
        case (x,y) of
            (a, _) => a [3]

(*2.naloga IZP1 2014/2015*)
fun tip a (b, c) = case (b, a) of (_, (SOME c)::x) => x | _ => tl a
(*val tip = fn : 'a option list -> 'b * 'c -> 'a option list*)

(*2.naloga IZP2 2014/2015*)
datatype 'a Tip = A of (int * 'a)
| B of ('a -> int)
fun mojamoja a b =
        case a of
            B x => B x
        | _ => A(3, 3)
(* val mojamoja = fn : int Tip -> 'a -> int Tip *)

datatype ('a,'b) chain =
    final
| Node of {a:'a ref, b:'b} * ('b,'a) chain;

(* fun chain_to_list veriga = 
        case veriga of
            final => ([],[])
        | Node ({a=val_a, b=val_b}, final) => ([!val_a],[b])
        | Node ({a=val_a, b=val_b}, Node({a=val_b2, b=val_a2} , nova_veriga)) =>
            let val (sez_a, sez_b) = chain_to_list nova_veriga
            in (!val_a::val_a2::sez_a, val_b::(!val_b2)::sez_b)
            end *)


(* funktor je funkcija slika strukture v strukturo ki se ujema s podpisom *)

structure Karte =
struct
    (* podatkovni tipi za predstavitev igralnih kart in množice kart v rokah *)
    datatype barva = Pik | Karo | Srce | Kriz
    datatype karta = Karta of (barva * int) | Joker
    type karte = string list
    (* seznam kart, ki jih držimo v rokah, val v_roki : karte ref *)
    val v_roki = ref []:karte ref
    (* izjema, ki se proži ob izdelavi primerka neveljavne karte *)
    exception NeveljavnaKarta of (barva * int)
    (* funkcija za izdelavo primerka nove karte, val nova_karta : barva * int -> karta *)
    fun nova_karta (barva, int) =
            if (int>=2 andalso int <=14) then Karta(barva,int) else raise NeveljavnaKarta(barva,int)
    (* funkcija za dodajanje nove karte v roke, val dodaj_v_roke : karta -> karte *)
    fun dodaj_v_roke (nova:karta) =
            let val count_jokers = List.foldl (fn (el,ac)=>if (el="Joker") then ac+1 else ac) 0 (!v_roki)
            in
                (case nova of
                        Joker => if (count_jokers <4) then v_roki := (!v_roki) @ ["Joker"] else ()
                    | _ => v_roki := (!v_roki) @ ["Karta"]
                    ; (!v_roki))
            end
    (* prikaže karte, ki jih imamo v rokah, val pokazi_roke : unit -> karte *)
    fun pokazi_roke () = (!v_roki)
end

(* karte onzaci s karta, jokerje z joker *)
(* 1) vpogled v svoje karte v roki
dostop do v_roki ali pokazi_roke
raje pokazi_roke kot v_roki, ker onemogočima dostop 

fun pokazi_roke () = (!v_roki) unit --> karte
type karte = string list

v podpisu type, za posamezne val
lahko type karte pa pokazi_roke:unit -> karte - skrijemo tip karte
ali pokazi_roke: unit -> string list - krajsi podpis

2) lahko doda novo karto
rabimo karta, potem rabimo barvo, rabimo Jokerja

3) ne damo exception zaradi krajsega podpisa
*)

signature KartePodpis =
sig
    type karta
    datatype barva = Pik | Kriz | Srce | Karo
    val Joker: karta
    val Karta: (barva*int) -> karta
    (* type karte = string list *)
    (* val neveljavnaKarta: (barva * int) exception *)
    val pokazi_roke: unit -> string list (* karte *)
    val dodaj_v_roke: karta -> string list (* karte *)
    val nova_karta: barva * int -> karta
end

(* Karte.dodaj_v_roke (Karte.nova_karta(Karte.Pik,10)
   Karte.dodaj_v_roke (Karte.Joker))
   Karte.dodaj_v_roke (Karte.nova_karta(Karte.Pik,15)
   Karte.Karta(Karte.Pik,10) 
   Karte.pokazi_roke () *)

(*4. naloga posplosi
kje se ujemajo, imajo case stavek vzamejo seznam, primerjajo če je prazen,
če ne vzamejo glavo in rep, razlike so tisto kar vrnemo
*)
fun first_op sez =
        case sez of
            nil => [true]
        | g::r => (not g)::(second_op r)
and second_op sez =
        case sez of
            nil => [false]
        | g::r => (g)::(third_op r)
and third_op sez =
        case sez of
            nil => nil
        | g::r => true::(first_op r)

fun op123 a fun1 fun2 sez=
    case sez of 
        nil => a      (* funkcija vrne konstanto a, a je drugi parameter*)
        | g::r => (fun1 g) :: (fun2 r)(*vzamejo glavo in na repu klicejo funkcijo, funkcija bo parameter f*)

(*implementiraj first_op*)
val first_op = op123 [true] (fn x => not x) second_op (* lahko tudi not, prva funkcija, ki dela z glavo, druga funkcija, ki dela z repom*)
val second_op = op123 [false] (fn x => x) third_op
val third_op = op123 nil (fn x => true) first_op (*ne more samo true, true ni funkcija*)

fun first_op (sez) = op123 [true] (fn x => not x) second_op sez (* lahko tudi not, prva funkcija, ki dela z glavo, druga funkcija, ki dela z repom*)
and second_op (sez) = op123 [false] (fn x => x) third_op sez
and third_op (sez) = op123 nil (fn x => true) first_op sez


(* *)
(* fun f1 c l1 l2 = if c then SOME (l1,l2) else NONE
fun f2 c l1 = if c then l1 else 0
fun f3 c l1 = if not c then SOME l1 else NONE

(* fun general = if ... then ... else  *)
(* fun general c l1 l2 = if ... then ... else   *)
(* fun general c l1 l2 = if fun1 c then ... else   c,c in not c->fun1*)
fun general fun1 fun2 a c l1 l2 = if fun1 c then fun2 l else a (* NONE, 0, NONE -> a*)

fun general = *)

(* 4) filter z foldl/foldr brez @
    foldl f acc list
    foldr f acc list
    map gre po vrsti in slika
    filter: funkcija ki filtrira in seznam na katerem izvajamo
    v accumulatorju hrani seznam, moramo limati na zacetek ker ne smemo @
    lazje List.foldr

    a je element ok za filter f na elementu
    (f x) true -> x::acc
    (f x) false -> acc 
*)

fun filter f sez = 
    List.foldr (fn (x,acc) => if (f x) then x::acc else acc) [] sez
