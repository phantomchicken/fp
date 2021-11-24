signature RATIONAL =
sig
    (* Definirajte podatkovni tip rational, ki podpira preverjanje enakosti. *)
    (*zelimo okrajsane ulomke in imenovalce > 1*)
    eqtype rational

    (* Definirajte izjemo, ki se kliče pri delu z neveljavnimi ulomki - deljenju z nič. *)
    exception BadRational

    (* Vrne racionalno število, ki je rezultat deljenja dveh podanih celih števil. *)
    val makeRational: int * int -> rational

    (* Vrne nasprotno vrednost podanega števila. *)
    val neg: rational -> rational

    (* Vrne obratno vrednost podanega števila. *)
    val inv: rational -> rational

    (* Funkcije za seštevanje in množenje. Rezultat vsake operacije naj sledi postavljenim pravilom. *)
    val add: rational * rational -> rational
    val mul: rational * rational -> rational

    (* Vrne niz, ki ustreza podanemu številu.
        Če je število celo, naj vrne niz oblike "x" oz. "~x".
        Če je število ulomek, naj vrne niz oblike "x/y" oz. "~x/y". *)
    val toString: rational -> string
end

structure Rational :> RATIONAL = (*implementira Rational, prekopiraj vse*)
struct
    (* Definirajte podatkovni tip rational, ki podpira preverjanje enakosti. *)
    (*zelimo okrajsane ulomke in imenovalce > 1*)
    type rational = int * int

    (* Definirajte izjemo, ki se kliče pri delu z neveljavnimi ulomki - deljenju z nič. *)
    exception BadRational

    fun gcd (a,0) = a
    | gcd (a,b) = gcd (b,a mod b)

    fun lcm (a,0) = 0
    | lcm (0,a) = 0
    | lcm (a,b) = a * b div gcd (a,b)

    (* Vrne racionalno število, ki je rezultat deljenja dveh podanih celih števil. *)
    fun makeRational (a,0) = raise BadRational
    | makeRational (a,b) = 
        let
            val d = gcd(a,b) (* skupni imenovalec*) 
            val (a,b) = (a div d, b div d)
        in 
            if b>0 then (a,b)
            else (~a,~b)
        end

    (* Vrne nasprotno vrednost podanega števila. *)
    fun neg (a,b) = (~a,b);

    (* Vrne obratno vrednost podanega števila. *)
    (* val inv: rational -> rational *)
    fun inv (a,b) = if a<0 then (~b,~a) else (b,a) 

    (* Funkcije za seštevanje in množenje. Rezultat vsake operacije naj sledi postavljenim pravilom. *)
    (* val add: rational * rational -> rational *)
    fun add ((a1,b1),(a2,b2)) = 
        let
            val lcm = lcm (b1,b2)
            val fact1 = lcm div b1
            val fact2 = lcm div b2
        in
            makeRational (a1 * fact1 + a2 * fact2, lcm)
        end

    (* val mul: rational * rational -> rational *)
    fun mul ((a1,b1),(a2,b2)) = makeRational (a1 * a2, b1 * b2) 

    (* Vrne niz, ki ustreza podanemu številu.
        Če je število celo, naj vrne niz oblike "x" oz. "~x".
        Če je število ulomek, naj vrne niz oblike "x/y" oz. "~x/y". *)
    fun toString (a,1) = Int.toString a
    | toString (a,b) = Int.toString a ^ "/" ^ Int.toString b
end

(* 
open Rational
makeRational(4,20) = makeRational (-1,-5) 
makeRational (4,20) skrito
toString it vidimo*)

signature EQ =
sig
    type t
    val eq : t -> t -> bool
end

signature SET =
sig
    (* podatkovni tip za elemente množice *)
    type item

    (* podatkovni tip množico *)
    type set

    (* prazna množica *)
    val empty : set

    (* vrne množico s samo podanim elementom *)
    val singleton : item -> set

    (* unija množic *)
    val union : set -> set -> set

    (* razlika množic (prva - druga) *)
    val difference : set -> set -> set

    (* a je prva množica podmnožica druge *)
    val subset : set -> set -> bool
end

funsig SETFN (Eq : EQ) = SET

(* functor SetFn  (E:EQ) : SET where type item = EQ.t =
struct
end *)

