val _ = Control.Print.printDepth := 10;
val _ = Control.Print.printLength := 10;
val _ = Control.Print.stringDepth := 2000;
val _ = Control.polyEqWarn := false;


fun readFile filename =
  let val is = TextIO.openIn filename
  in 
    String.map (fn c => if Char.isGraph c orelse c = #" " orelse c = #"\n" then c else #" ")
      (TextIO.inputAll is)
    before TextIO.closeIn is
  end

exception NotImplemented;

(* robni primer [] vrne []
   splitAt naredi tocno kar zelimo, ce ne uspe sprozi izjemo in vrne ([],[])
   ce je rezultat [],[] vemo da ni uspelo, sicer vzamemo glavo rezultata in klicemo rekurzivno na repu
*)
fun split _ [] = [] 
| split blockSize xs =
      let
        val tmp = List.splitAt(xs,blockSize) handle Subscript => ([],[])
      in 
        case tmp of 
        	([],[]) => [] 
          | (_,_) => #1 (tmp) :: (split blockSize (#2 tmp))
      end

signature RING =
sig
  eqtype t
  val zero : t
  val one : t
  val neg : t -> t
  val inv : t -> t option
  val + : t * t -> t
  val * : t * t -> t
end;

functor Ring (val n : int) :> RING where type t = int =
struct
  type t = int
  val zero = 0
  val one = 1
  fun neg x = ~x mod n


  (* naivni ampak delujoci nacin, 
     i zacne od n in se zmanjsuje do 0,
     ce velja a * !i mod n = 1,
     nasli smo inverz po modulu, ki je kar i *)
  fun modularInverseBrute (n, a) =
    let
    val i = ref n
    val rez = ref ~1
    val _ = while (!i > 0) do (
        i := (!i - 1);
        if (a * !i mod n = 1) 
        then 
            rez := !i 
        else ()
    )
    in
        !rez
    end;

  fun modularInverse (a, n) =  if ((modularInverseBrute (n,a)) = ~1) (*~1 nismo nasli inverza*)
                               then NONE 
                               else SOME (modularInverseBrute (n,a));
                               
  fun inv x = modularInverse (x mod n, n)
  fun op + a =  Int.+ a mod n
  fun op * p =  Int.* p mod n
end;

signature MAT =
sig
  eqtype t
  structure Vec :
    sig
      val dot : t list -> t list -> t
      val add : t list -> t list -> t list
      val sub : t list -> t list -> t list
      val scale : t -> t list -> t list
    end
  val tr : t list list -> t list list
  val mul : t list list -> t list list -> t list list
  val id : int -> t list list
  val join : t list list -> t list list -> t list list
  val inv : t list list -> t list list option
end;

fun join [] [] = []
| join xs [] = xs
| join [] ys = ys
| join xs ys = let val glava = hd xs @ hd ys; val rep = join (tl xs) (tl ys); (* zdruzi hd v glava, shrani rezultat rekurzije v rep, zdruzi glavo in rep*)
                   in glava::rep
                   end;

functor Mat (R : RING) :> MAT where type t = R.t =
struct
  type t = R.t
  structure Vec =
    struct
      fun dot v1 v2 = foldl (fn (x,y) => R.+(x,y)) R.zero (ListPair.map(fn(x,y)=> R.*(x,y)) (v1,v2));
      
      fun add [] x = x 
      | add x [] = x
      | add x y = let 
                    val (a,b) = hd (ListPair.zip (x,y)) (*iz dveh seznamov tvori seznam terk*)
                  in (R.+(a,b))::add (tl x) (tl y)      (* sestajemo glavi in rekurzivno nadaljujemo na repu*)
                  end;

      fun sub [] x = x 
      | sub x [] = x
      | sub x y = let 
                    val (a,b) = hd (ListPair.zip (x,y))
                  in (R.+(a, R.neg b))::sub (tl x) (tl y)
                  end;
      
      fun scale skalar [] = [] 
      | scale skalar xs = List.map (fn (x) => R.*(x,skalar)) xs   

    end

  (* [1 2 3]
     [4 5 6]
     tabulate: n, [f(0) f(1) f(2)]

     n = 3 aplikacije (toliko aplikacij kolikor je dolga vrstica)
     j = vrstice [1 2 3], [4 5 6]
     i = stevilka aplikacije, indeks elementa vrstice

     f(0) = List.nth([1 2 3],0) :: List.nth([4 5 6],0) = [1 4]
     f(1) = List.nth([1 2 3],1) :: List.nth([4 5 6],1) = [2 5]
     f(2) = List.nth([1 2 3],2) :: List.nth([4 5 6],2) = [3 6]
   *)
  fun tr [[]] = [[]]
  | tr [] = []
  | tr (matrix) =
    List.tabulate (List.length (List.nth (matrix, 0)), fn i => List.map (fn j => List.nth (j, i)) matrix);

  (* zacetna vrednost je 0
  ListPair.map zmnozi elemente seznamov = zacetni seznam
  sestejemo zmnozke z foldl *)

  fun dot v1 v2 = foldl (fn (x,y) => R.+(x,y)) R.zero (ListPair.map(fn(x,y)=> R.*(x,y)) (v1,v2));

  (*vrstice produkta Mi-vr = Ai-vr dot Bi-st *)
  fun mul m1 m2 =
    case m1 of 
        [] => []
        | x::xs => [List.map (fn y => dot x y) (tr m2)] @ (mul xs m2);  

  (*zeroRow n ustvari vrstico/seznam z n nicel
    replaceZeroAtIndex na mestu i v seznamu nastavi 1
    
    identiteta = prefiks @ sufiks
    
    prefiks je vrstica nicel, sufiks je vrstica z 1 na zacetku
    prefiks zacne z dolzino 0 in se povecuje
    sufiks zacne z dolzino n in se zmanjsuje

      |1 0 0
       0|1 0
       0 0|1
    *)

  fun id n = 
    let
      fun zeroRow 0 = nil
      | zeroRow n = (R.zero)::(zeroRow (n-1))  
  
      fun replaceZeroAtIndex (i, ix, []) = []
      | replaceZeroAtIndex (i, ix, x::xs) = (if i = ix then R.one else R.zero) :: (replaceZeroAtIndex (i+1, ix, xs))

      fun idHelper (_,0) = [] 
      | idHelper (i, n) = 
        let 
            val prefix = zeroRow i
            val suffix = replaceZeroAtIndex (1,1,zeroRow n)
            val row = (prefix) @ (suffix)
        in
            (row)::(idHelper(i+1,n-1)) 
        end;

    in idHelper (0,n)
    end
   
  (* zdruzimo glavi in shranimo v glava, shranimo rezultat rekurzije v rep, staknemo glavo in rep*)
  fun join [] [] = []
  | join xs [] = xs
  | join [] ys = ys
  | join xs ys = let val glava = hd xs @ hd ys; 
                     val rep = join (tl xs) (tl ys);
                 in glava::rep
                 end;

  (* odsteje vrednosti vektorja v vsem vrsticam v matriki, da so v prvem stolpcu same 0 *)
  fun reduce v m = 
    map (fn x :: xs =>(Vec.sub xs o Vec.scale x) v ) m
  
  (* poisce vrstico, ki ima na začetku inverzibilno vrednost, in jo pomnozi s inverzom. Rezultat je matrika, ki ima na vrhu vrstico, ki se zacne z 1*)
  fun pivot ((v as x::xs) :: m) = 
    (case R.inv x of 
      SOME x' => SOME (Vec.scale x' v :: m) 
      | NONE => case pivot m of
                  SOME (v' :: m') => SOME (v' :: v :: m')
                  | _ => NONE)
      | pivot _ = NONE
  
  (* ce je curr [] vrni above
     klici pivot na curr, 
     ce je oblike (_::v)::m nadaljuj rekurzijo, reduciraj above z v-jem, reduciraj m z v-jem,
     ce je NONE => [],
     sicer vrni kar pivot curr
   *)
  fun gauss (above, []) = above
    | gauss (above, curr) =
      case pivot curr of
        SOME ((_::v)::m) => gauss (reduce v above @ [v], reduce v m) (*reduciramo above z v-jem*)
        | NONE => []
        | _ => valOf(pivot curr)

  (* klice gauss na ([], [M|I]) -- above je []
     ce je rezultat i, neprazen, kvadraten seznam seznamov/matrika, vrni i
  *)
  fun inv m = let  
                val i = gauss ([], join m (id (List.length m))); 
              in 
                 if not (i=[]) andalso not (null i) andalso not (null (hd i)) andalso (List.length i) = (List.length (hd i))
                 then SOME i 
                 else NONE 
              end

end;


signature CIPHER =
sig
  type t
  val encrypt : t list list -> t list -> t list
  val decrypt : t list list -> t list -> t list option
  val knownPlaintextAttack : int -> t list -> t list -> t list list option
end;

fun matrixToList m = foldr (fn(x,acc)=> x @ acc) [] m
   
functor HillCipherAnalyzer (M : MAT) :> CIPHER
  where type t = M.t
=
struct
  type t = M.t
  
  fun matrixToList m = 
    case m of g::r => g @ matrixToList r
    | _ => []

  (* r = razbijemo plaintext na l (velikost kljuca) podmnozic 
     matricno mnozimo r in kljuc *)
  fun encrypt key plaintext =
    let
      val l = List.length key
      val r = split l plaintext
    in matrixToList(M.mul r key)
    end

  (* podobno le da mnozimo z inverzom kljuca, ce le-ta obstaja*)
  fun decrypt key ciphertext =
    let
      val l = List.length key
      val r = split l ciphertext
      val keyInverse = (M.inv key)
    in if isSome keyInverse then SOME (matrixToList(M.mul r (valOf(keyInverse)))) else NONE
    end

  (* razkosamo plaintext in ciphertext na toliko kosov koliko ima kljuc = x,y
     ce je dolzina blokov x in y >= dolzina kljuca IN obstaja inverz bloka x dolzine keyLength 
     potem je kljuc = matricni zmnozek inverza bloka x, z blokom y
     sicer rekurzivno klici rep
     ce smo dobili kljuc k, potem preverimo na vseh elementih, map: matricno mnozi kos x s kljucem. Dobljeni rezultat mora biti natanko seznam kosov y
  *)

  fun knownPlaintextAttack keyLength plaintext ciphertext =   
    let
      val x = split keyLength plaintext
      val y = split keyLength ciphertext
      fun plainTextHelper xBlok yBlok keyLength =
        case (xBlok,yBlok) of
          (x::xs,y::ys) => if (List.length xBlok >= keyLength) andalso (List.length yBlok >= keyLength) andalso isSome (M.inv (List.take (xBlok, keyLength))) 
                          then  SOME (M.mul (valOf(M.inv (List.take (xBlok, keyLength)))) (List.take (yBlok, keyLength))) 
                          else plainTextHelper xs ys keyLength
          | (_,_) => NONE
      val k = plainTextHelper x y keyLength
    in if isSome k andalso ( (matrixToList (List.map (fn(el)=> M.mul [el] (valOf (k))  ) x)) = y )
       then k else NONE
    end 
      
end;


structure Trie :> 
sig
eqtype ''a dict
val empty : ''a dict
val insert : ''a list -> ''a dict -> ''a dict
val lookup : ''a list -> ''a dict -> bool
end
=
struct
  datatype ''a tree = N of ''a * bool * ''a tree list
  type ''a dict = ''a tree list

  val empty = [] : ''a dict

  (* pomozna funkcija za prazen slovar = napolni slovar z eno samo besedo w*)
  fun makeDict w = 
  case w of 
        [w] => [N(w, true, [])]
        | w::ws => [N(w, false, makeDict ws)]       

  (*
  1) zadnja crka, slovar = ce element slovarja vsebuje crko, nastavi jeKonec na true in ga vrni, sicer samo vrni
  2) beseda, prazen slovar = napolni slovar z besedo (makeDict funkcija)
  3) beseda, slovar = ce element slovarja vsebuje crko, vrni element slovarja v katerem rekurzivno nadaljujemo na "rep" @ rep2, sicer vrni element slovarja @ rekurzivno nadaljuj rep2
  *)

  fun insert word dict = 
  case (word,dict) of 
        ([w], N(crka, jeKonec, rep)::rep2) => if w=crka then N(crka, true, rep)::rep2 else N(crka, false, rep)::rep2
        | (w::ws, []) => makeDict (w::ws)
        | (w::ws, N(crka, jeKonec, rep)::rep2) => if w = crka then [N(crka, jeKonec, (insert ws rep))] @ rep2 else [N(crka, jeKonec, rep)] @ (insert word rep2)

  (*
  1) zadnja crka, slovar = ce element slovarja vsebuje crko in je koncna (true) potem smo nasli besedo, sicer nismo
  2) beseda, slovar = ce element slovarja vsebuje crko rekurzivno isci v "rep" ALI rekurzivno isci v "rep", sicer rekurzivno isci v "rep2" 
  3) beseda, konec slovarja = nismo nasli besede
  4) konec besede, _ = nismo nasli besedo
  *)
  fun lookup w dict =
  case (w,dict) of 
      (w::[], N(crka, jeKonec, rep)::rep2) => if w = crka andalso jeKonec then true else false
      | (w::ws, N(crka, jeKonec, rep)::rep2) => if w = crka then lookup ws rep orelse lookup ws rep2 else lookup (w::ws) rep2
      | (w::ws, []) => false
      | ([], _) => false

end;

signature HILLCIPHER =
sig
  structure Ring : RING where type t = int
  structure Matrix : MAT where type t = Ring.t
  structure Cipher : CIPHER where type t = Matrix.t
  val alphabetSize : int
  val alphabet : char list
  val encode : string -> Cipher.t list
  val decode : Cipher.t list -> string
  val encrypt : Cipher.t list list -> string -> string
  val decrypt : Cipher.t list list -> string -> string option
  val knownPlaintextAttack :
      int -> string -> string -> Cipher.t list list option
  val ciphertextOnlyAttack : int -> string -> Cipher.t list list option
end

val alphabet =
  [#" ",#"a",#"b",#"c",#"d",#"e",#"f",#"g",#"h",#"i",#"j",#"k",#"l",#"m",#"n",
   #"o",#"p",#"q",#"r",#"s",#"t",#"u",#"v",#"w",#"x",#"y",#"z"]

(* ce je glava seznama nasa crka, nasli smo indeks crke - vrni i, sicer rekurzivno nadaljuj
   [] => nismo nasli, vrni -1  *)

fun najdiIxCrke (alphabet, crka, i) =
  case alphabet of
    [] => ~1
    | x::xs => if x=crka then i else najdiIxCrke (xs, crka, i+1)

functor HillCipher (val alphabet : string) :> HILLCIPHER =
struct

(*printable characters*)
val alphabetSize = String.size alphabet
val alphabet = String.explode alphabet

structure Ring = Ring (val n = alphabetSize)
structure Matrix = Mat (Ring)
structure Cipher = HillCipherAnalyzer (Matrix)

(* txtChar = razbijemo string na chare
   poisci indeks crke iz txtChar v slovarju @ rekurzivno klici na repu, ki ga sestavimo skupaj s String.implode
   ce crke ni, sprozimo izjemo
*)

fun encode txt =
  let 
    val txtChar = String.explode (txt)
    fun najdiIxCrke (alphabet, crka, i) =
      case alphabet of
        [] => ~1
        | x::xs => if x=crka then i else najdiIxCrke (xs, crka, i+1) 
  in 
    case txtChar of 
      [] => []
      | x::xs => if ((najdiIxCrke (alphabet,x,0)) = ~1) 
                 then raise NotImplemented 
                 else [(najdiIxCrke (alphabet,x,0))] @ (encode (String.implode(xs)))
  end

(* vrni i-ti element slovarja @ rekurzivno rep
   ce je slovar manjsi od i, sprozi izjemo
*)

fun decode code = 
  let 
    fun decodeHelper code = 
      case code of 
        [] => []
        | x::xs => if (List.length(alphabet)<x) 
                  then raise NotImplemented 
                  else [List.nth(alphabet,x)] @ (decodeHelper xs)
  in String.implode(decodeHelper (code))
  end 

local
  fun parseWords filename =
    let val is = TextIO.openIn filename
      fun read_lines is =
        case TextIO.inputLine is of
          SOME line =>
            if String.size line > 1
            then String.tokens (not o Char.isAlpha) line @ read_lines is
            else read_lines is
          | NONE => []
    in List.map (String.map Char.toLower) (read_lines is) before TextIO.closeIn is end

  val dictionary = List.foldl (fn (w, d) => Trie.insert w d) Trie.empty (List.map String.explode (parseWords "hamlet.txt")) handle NotImplemented => Trie.empty
in

  (* sifriraj plaintext in razsekaj na toliko kosov, koliko je dolg kljuc
     matricno mnozi sifrirani kos plaintexta * kljuc @ rekurzivno rep
     desifriraj sifrirani rezultat (List.concat spravi seznam seznamov v seznam [[]] => []) *)

  fun encrypt key plaintext =
    let
      val kosi = split (List.length(key)) (encode(plaintext))
      fun helper kosi = 
        case kosi of 
          [] => []
          | x::xs => (Matrix.mul [x] key) @ (helper xs)
      val sifrirano = helper kosi 
    in decode (List.concat sifrirano)
    end


  (* sifriraj ciphertext in razsekaj na toliko kosov, koliko je dolg kljuc
     matricno mnozi sifrirani kos ciphertexta * inverz kljuca @ rekurzivno rep
     desifriraj sifrirani rezultat (List.concat spravi seznam seznamov v seznam [[]] => []) 
     vse se izvede le ce obstaja inverz kljuca
     *)

  fun decrypt key ciphertext = let
      val kosi = split (List.length(key)) (encode(ciphertext))
      val keyInverse = Matrix.inv key
      fun helper kosi = 
        case kosi of 
          [] => []
          | x::xs => (Matrix.mul [x] (valOf(keyInverse))) 
          @ (helper xs)
    in if isSome (keyInverse) 
       then SOME (decode (List.concat (helper kosi))) 
       else NONE
    end

  (* sifriraj plaintext in ciphertext ter poklici knownPlaintextAttack*)
  fun knownPlaintextAttack keyLength plaintext ciphertext = Cipher.knownPlaintextAttack keyLength (encode plaintext) (encode ciphertext)
  fun ciphertextOnlyAttack keyLength ciphertext = raise NotImplemented
  end
end;
