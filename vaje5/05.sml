signature RATIONAL =
sig
    (* Definirajte podatkovni tip rational, ki podpira preverjanje enakosti. *)
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