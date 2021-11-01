datatype barva = Kriz | Pik | Srce | Karo
datatype stopnja = As | Kralj | Kraljica | Fant | Stevilka of int

type karta = stopnja * barva

(* Kakšne barve je karta? *)
fun barvaKarte (k : karta) : barva = 
        case k of (_,b) => b

fun barvaKarte' ((_,b)) =b;


(* Ali je karta veljavna? *)
fun veljavnaKarta (k : karta) : bool =
        case k of
            (Stevilka s,_) => s>=2 andalso s<=10
        | (_,_) => true;

fun veljavnaKarta' ((Stevilka s,_) : karta) : bool = s>=2 andalso s<=10
    |  veljavnaKarta' _ = true;


(* Koliko je vredna karta? *)
fun vrednostKarte (k : karta) : int =
        let
            val stopnja = #1 k
        in
            case stopnja of
                As => 11
            | Kralj => 14
            | Kraljica => 13
            | Fant => 12
            | Stevilka s => s        
        end


fun vrednostKarte' ((s,b) : karta) : int =
        case s of
            Stevilka x => x
        | As => 11
        | _ => 10;



(* Kolikšna je vrednost vseh kart v roki? *)  
fun vsotaKart (ks : karta list) : int = 
        case ks of 
            nil => 0
        | (g::r) => vrednostKarte'(hd ks) + vsotaKart(tl ks)

fun vsotaKart' (nil: karta list): int  = 0 | 
    vsotaKart' (k::ks) = vrednostKarte' k + vsotaKart' ks

(* Ali imam v roki karte iste barve? *)
fun isteBarve ((_,b1)::(s2,b2)::ks) : bool =
        b1=b2 andalso isteBarve((s2,b2)::ks)
    | isteBarve(_) = true;

fun isteBarve' ((_,b1):: (r as (s2,b2)::ks)) : bool =
        b1=b2 andalso isteBarve' r
    | isteBarve'(_) = true;


datatype bstree = br of bstree * int * bstree | lf of int;
type name = {first : string, last : string};
type date = {month : string, day : int, year : int};
type person = {name : name, birthdate : date};

(* Vrne true, če seznam `ls` predstavlja nepadajoče zaporedje. *)
fun isSorted (ls : int list) : bool =
        case ls of
            g1::g2::r => g1<=g2 andalso isSorted(g2::r)
            | _ => true; 

(* Vrne true, če drevo `tree` predstavlja veljavno kopico. *)
(* fun validHeap (tree : bstree) : bool = 
    case bstree of
        lf => true
        |  (l, s, d) => s>=(#2 l) andalso s>=(#2 d) andalso validHeap(l) andalso validHeap(d) *)

(* fun validHeap' (lf x) = true 
| validHeap' (br (lf l,s, lf d)) =  s>=l andalso s>=d andalso validHeap'(l) andalso validHeap'(d)
| validHeap' (br (br l,s, lf d)) =  s>=(#2 l) andalso s>=d andalso validHeap'(l) andalso validHeap'(d)
| validHeap' (br (lf l,s,br d)) =  s>=l andalso s>=br d andalso validHeap'(l) andalso validHeap'(d)
| validHeap' (br (br l,s,br d)) =  s>=(#2 l) andalso s>=(#2 d) andalso validHeap'(l) andalso validHeap'(d) *)

val tree5 = br (br (br (lf 1, 2, br (lf 3, 4, lf 5)), 6, lf 7), 8, br(lf 9, 10, lf 11));

fun validHeap (bst : bstree) : bool = 
    case bst of
        lf x => true
        | br (lf l, s, lf d) => s>l andalso s<d
        | br (lf l, s, br d) => s>l andalso s<(#2 d) andalso validHeap(br(#1 d, #2 d, #3 d)) 
        | br (br l, s, lf d) => s<d andalso s>(#2 l) andalso validHeap(br(#1 l, #2 l, #3 l))
        | br (br l, s, br d) => s>(#2 l) andalso s<(#2 d) andalso validHeap(br(#1 l, #2 l, #3 l)) andalso validHeap(br(#1 d, #2 d, #3 d))

(* Vrne ime najstarejše osebe iz seznama `people` kot opcijo (ker ni nujno, da obstaja). *)
(* fun oldest (people : person list) : name option = 
    case people of
    [x] => SOME x
    | (person g1):: (person g2)::r => SOME oldest(g2::r)  *)