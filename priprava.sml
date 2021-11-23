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