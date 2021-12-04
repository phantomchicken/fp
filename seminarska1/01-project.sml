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

fun split _ [] = [] 
| split blockSize xs =
    if blockSize > length xs 
    then [] 
    else 
      let
        val tmp = List.splitAt(xs,blockSize)
      in [#1 (tmp)] @ (split blockSize (#2 tmp))
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

  fun modularInverse (a, n) =  if ((modularInverseBrute (n,a)) = ~1) 
                               then NONE 
                               else SOME (modularInverseBrute (n,a));


 (* val test3 = Z96.inv 11 = SOME 35 *)
 (* a inv n = reš
 reš * n mod a = 1 *)
 (* 11 * 35 mod 96 = 1 *)
  (* fun foo (n, a) =
    let
    val counter = ref a
    in
    while !i > 0 do (
        if (n * i mod a) 
        then i 
        else
        i := !i - 1
    )
    end; *)

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
| join xs ys = let val glava = hd xs @ hd ys; val rep = join (tl xs) (tl ys);
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
                    val (a,b) = hd (ListPair.zip (x,y))
                  in (R.+(a,b))::add (tl x) (tl y)
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

  fun tr [[]] = [[]]
  | tr [] = []
  | tr (matrix) =
    List.tabulate (List.length (List.nth (matrix, 0)), fn i => List.map (fn j => List.nth (j, i)) matrix);

  fun dot v1 v2 = foldl (fn (x,y) => R.+(x,y)) R.zero (ListPair.map(fn(x,y)=> R.*(x,y)) (v1,v2));

  fun mul matrix1 matrix2 =
  case matrix1 of 
      [] => []
      |    x::xr => [List.map (fn y => dot x y) (tr matrix2)] @ (mul xr matrix2);  

  
  fun zeroRow 0 = nil
  | zeroRow n = (R.zero)::(zeroRow (n-1))  
  
  (* fun zeroMatrix n = List.tabulate(n, fn (x) => zeroRow n);   *)
  
  fun replaceZeroAtIndex (i, ix, []) = []
  | replaceZeroAtIndex (i, ix, x::xs) = (if i = ix then R.one else R.zero) :: (replaceZeroAtIndex (i+1, ix, xs))   
  
  (* - (zeroRow 0)@(replaceZeroAtIndex (1,1,zeroRow 3));;
  val it = [1,0,0] : int list
  - (zeroRow 1)@(replaceZeroAtIndex (1,1,zeroRow 2));;
  val it = [0,1,0] : int list
  - (zeroRow 2)@(replaceZeroAtIndex (1,1,zeroRow 1));;
  val it = [0,0,1] : int list *)  
  
  fun idHelper (_,0) = [] 
  | idHelper (i, n) = 
    let 
        val prefix = zeroRow i
        val suffix = replaceZeroAtIndex (1,1,zeroRow n)
        val row = (prefix) @ (suffix)
    in
        (row)::(idHelper(i+1,n-1)) 
    end;

  fun id n =  idHelper (0,n);

  fun join [] [] = []
  | join xs [] = xs
  | join [] ys = ys
  | join xs ys = let val glava = hd xs @ hd ys; 
                     val rep = join (tl xs) (tl ys);
                 in glava::rep
                 end;


  fun appendIdentityToMatrix m =
    let 
      val n = List.length m
      val idN = id n
    in idN
    end

  fun reduce v m = 
    map (fn x :: xs =>(Vec.sub xs o Vec.scale x) v ) m
  
  fun pivot ((v as x::xs) :: m) = 
    (case R.inv x of 
      SOME x' => SOME (Vec.scale x' v :: m) 
      | NONE => case pivot m of
                  SOME (v' :: m') => SOME (v' :: v :: m')
                  | _ => NONE)
      | pivot _ = NONE
  
  fun gauss (above, []) = above
    | gauss (above, curr) =
      case pivot curr of
        SOME ((_::v)::m) => gauss (reduce v above @ [v], reduce v m) (*reducitamo above z v-jem*)
        | NONE => []
        | _ => valOf(pivot curr)
        (* | NONE => NONE pivot
        | _ => SOME pivot *)

  fun inv m = let  
                val i = gauss ([], join m (id (List.length m))); 
              in 
                 if not (i=[]) andalso not (null i) andalso not (null (hd i)) andalso (List.length i) = (List.length (hd i))
                 then SOME i 
                 else NONE 
              end

  (* fun inv m = 
    case List.length m of
      1 => if isSome (R.inv (hd(hd m))) then SOME [[valOf(R.inv (hd(hd m)))]] else NONE
      | 2 => (case m of 
              [[a,b],[c,d]] => (let 
                                  val det = R.+ ((R.*(a,d), R.neg(R.*(b,c))))
                                  val detInv = R.inv(det) 
                                in (if isSome(detInv) then SOME [[R.*(valOf(detInv),d),R.*(valOf(detInv),R.neg(b))],[R.*(valOf(detInv),R.neg(c)),R.*(valOf(detInv),a)]] else NONE)  
                                end) 
              | _ => NONE)
      | _ => NONE  *)
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
  
  (* - C.encrypt [[1,0,0],[0,3,0],[0,0,1]] [1,2,3,4,5,6,7,8];
  val it = [1,6,3,4,5,6] : C.t list
  K:l x l matrika
  x: text
  r: razkosaj x po l [1,2,3],[4,5,6],[7,8] = [1,2,3],[4,5,6]
  mnozenje matrik  r * k = [[1,6,3],[4,15,6]] *)

  fun matrixToList m = 
    case m of g::r => g @ matrixToList r
    | _ => []

  fun encrypt key plaintext =
    let
      val l = List.length key
      val r = split l plaintext
    in matrixToList(M.mul r key)
    end

  fun decrypt key ciphertext =
    let
      val l = List.length key
      val r = split l ciphertext
      val keyInverse = (M.inv key)
    in if isSome keyInverse then SOME (matrixToList(M.mul r (valOf(keyInverse)))) else NONE
    end

  (* fun knownPlaintextAttack keyLength plaintext ciphertext =   
  let
    val x = split keyLength plaintext
    val y = split keyLength ciphertext
    val xInv = M.inv x
  in if isSome xInv then SOME (M.mul (valOf(xInv)) y) else NONE 
  end *)
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
    
     (*List.map (fn(el)=> M.mul [el] (valOf (k)) ) x;*)
   
  
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

  fun makeDict w = 
  case w of 
        [w] => [N(w, true, [])]
        | w::ws => [N(w, false, makeDict ws)]       

  fun insert word dict = 
  case (word,dict) of 
        ([w], N(crka, jeKonec, rep)::rep2) => if w=crka then N(crka, true, rep)::rep2 else N(crka, false, rep)::rep2
        | (w::ws, []) => makeDict (w::ws)
        | (w::ws, N(crka, jeKonec, rep)::rep2) => if w = crka then [N(crka, jeKonec, (insert ws rep))] @ rep2 else [N(crka, jeKonec, rep)] @ (insert word rep2)

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

fun najdiIxCrke (alphabet, crka, i) =
  case alphabet of
    [] => ~1
    | x::xs => if x=crka then i else najdiIxCrke (xs, crka, i+1)

fun encode txt =
  let 
    val txtChar = String.explode (txt) 
  in 
    case txtChar of 
      [] => []
      | x::xs => if ((najdiIxCrke (alphabet,x,0)) = ~1) 
                 then raise NotImplemented 
                 else [(najdiIxCrke (alphabet,x,0))] @ (encode (String.implode(xs)))
  end

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

  fun knownPlaintextAttack keyLength plaintext ciphertext = Cipher.knownPlaintextAttack keyLength (encode plaintext) (encode ciphertext)
  fun ciphertextOnlyAttack keyLength ciphertext = raise NotImplemented
  end
end;
