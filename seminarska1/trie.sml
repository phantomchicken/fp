val _ = Control.Print.printDepth := 10;
val _ = Control.Print.printLength := 10;
val _ = Control.Print.stringDepth := 2000;
val _ = Control.polyEqWarn := false;

exception NotImplemented;
structure Trie =
struct
  datatype ''a tree = N of ''a * bool * ''a tree list
  type ''a dict = ''a tree list

  (* - val t = Trie.insert (String.explode "Anamaria") Trie.empty;
    val t = [N (#"A",false, [N (#"n",false, [N (#"a",false, [N (#"m",false, [N (#"a",false, [N (#"r",false,[N (#"i",false,[N (#"a",true,[])])])])])])])])]
  : char Trie.tree list *)

  val empty = [] : ''a dict;
  (* fun insert [] d = d
  | insert (g::r) d =  d @ [N (g,true,insert t dict)];  *)

  fun findPlace (x::xs) (y::ys) = if x=y then findPlace xs ys else ys
  fun insertHelper [] dict = [N(#"z",false,[])]
        | insertHelper (g::nil) dict = [N(g,true,[])]
        | insertHelper (g::r) dict = dict @ [N(g,false,insertHelper r dict)]

  fun insert' (x::xs) (y::ys) = [N(x,false,insert' xs ys)] (*if x=y then insertHelper (xs) (ys) else*)
  | insert' (x::xs) empty = [N(x,false,insert' xs [x])]
  |  insert' [] ys = empty 
(* https://stackoverflow.com/questions/56228958/inserting-values-in-a-trie *)
  (* >Anamaria
  A n a m a r i a(t)
  >Ana
  A n a (t) m a r i a(t)
  >Alice
  A n a (t) m a r i a(t)
    l i c e (t)
  >Alan
  A n a (t) m a r i a(t)
    l i c e (t)
      a n (t)
  >A
  A(t) n a (t) m a r i a(t)
    l i c e (t)
      a n (t) *)

  (*foldl (fn(el,acc) => el::acc) empty w*)

  (* fun insert w dict =
    if dict = empty then (insertHelper w dict)
    else insertHelper' w dict *)

    
    
    (* :: [N (g,true,insert r dict)];*) 
    
    (*Trie.empty @ [Trie.N("a",true,[])]; 
val it = [N ("a",true,[])] : string Trie.tree list*)
  
  (*raise NotImplemented*)
  

    
  fun lookup word dict = raise NotImplemented;
end

(* - Trie.insert;
val it = fn : ''a list -> ''a Trie.dict -> ''a Trie.dict
- val t = Trie.insert (String.explode "Anamaria") Trie.empty;
val t =
  [N (#"A",false, [N (#"n",false, [N (#"a",false, [N (#"m",false, [N (#"a",false, [N (#"r",false,[N (#"i",false,[N (#"a",true,[])])])])])])])])]
  : char Trie.tree list
- val t = Trie.insert (String.explode "Ana") t;
val t =
  [N (#"A",false, [N (#"n",false, [N (#"a",true, [N (#"m",false, [N (#"a",false, [N (#"r",false,[N (#"i",false,[N (#"a",true,[])])])])])])])])]
  : char Trie.tree list
- val t = Trie.insert (String.explode "Alice") t; val t = [N (#"A",false, [N (#"n",false, [N (#"a",true, [N (#"m",false, [N (#"a",false, [N (#"r",false,[N (#"i",false,[N (#"a",true,[])])])])])])]), N (#"l",false,[N (#"i",false,[N (#"c",false,[N (#"e",true,[])])])])])]
  : char Trie.tree list
- val t = Trie.insert (String.explode "Alan") t;
val t = [N (#"A",false, [N (#"n",false, [N (#"a",true, [N (#"m",false, [N (#"a",false, [N (#"r",false,[N (#"i",false,[N (#"a",true,[])])])])])])]), N (#"l",false, [N (#"i",false,[N (#"c",false,[N (#"e",true,[])])]), N (#"a",false,[N (#"n",true,[])])])])]
  : char Trie.tree list
- val t = Trie.insert [#"A"] t;
val t = [N (#"A",true, [N (#"n",false, [N (#"a",true, [N (#"m",false, [N (#"a",false, [N (#"r",false,[N (#"i",false,[N (#"a",true,[])])])])])])]), N (#"l",false, [N (#"i",false,[N (#"c",false,[N (#"e",true,[])])]), N (#"a",false,[N (#"n",true,[])])])])]
  : char Trie.tree list *)



  fun zeroRow 0 = nil
  | zeroRow n = (0)::(zeroRow (n-1))  
  
  fun replaceZeroAtIndex (i, ix, []) = []
  | replaceZeroAtIndex (i, ix, x::xs) = (if i = ix then 1 else 0) :: (replaceZeroAtIndex (i+1, ix, xs))   
  
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
      fun appendMatrices (x::xs, y::ys) = (x@y) :: (appendMatrices (xs, ys))
      | appendMatrices (_,_) = []
      
    in appendMatrices (m, idN)
    end