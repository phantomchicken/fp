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

  val dict = [N (#"A",true, [N (#"n",false, [N (#"a",true, [N (#"m",false, [N (#"a",false, [N (#"r",false,[N (#"i",false,[N (#"a",true,[])])])])])])])
                            , N (#"l",false, [N (#"i",false,[N (#"c",false,[N (#"e",true,[])])])
                            , N (#"a",false,[N (#"n",true,[])])])])];

  fun lookup w dict =
    case (w,dict) of 
        (w::[], N(crka, jeKonec, rep)::rep2) => if w = crka andalso jeKonec then true else false
        | (w::ws, N(crka, jeKonec, rep)::rep2) => if w = crka then lookup ws rep orelse lookup ws rep2 else lookup (w::ws) rep2
        | (w::ws, []) => false
        | ([], _) => false

  
  fun makeDict w = 
    case w of 
        [w] => [N(w, true, [])]
        | w::ws => [N(w, false, makeDict ws)]
        

  fun insert w dict = 
    case (w,dict) of 
          (w::ws, N(crka, jeKonec, rep)::rep2) => if w = crka then dict @ (insert ws rep) else dict @ (insert (w::ws) rep2)
          | (w::ws, []) => makeDict (w::ws)
          | ([], N(crka, jeKonec, rep)::rep2) => N(crka, true, rep)::rep2

  
  (* fun findPlace (x,  N(y,jeZe,r)) = if x=y then y else findPlace (x, r)
  fun insertHelper [] dict = [N(#"z",false,[])]
        | insertHelper (g::nil) dict = [N(g,true,[])]
        | insertHelper (g::r) dict = dict @ [N(g,false,insertHelper r dict)]; *)

  (* fun insert' (x::xs) (y::ys) = N(x,false,insert' xs ys) (*if x=y then insertHelper (xs) (ys) else*)
  | insert' (x::xs) empty = N(x,false,insert' xs [x])
  |  insert' [] ys = ys  *)
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

  (* fun insert w dict =
    if dict = empty then (insertHelper w dict)
    else insertHelper' w dict *)

    
    
    (* :: [N (g,true,insert r dict)];*) 
    
    (*Trie.empty @ [Trie.N("a",true,[])]; 
val it = [N ("a",true,[])] : string Trie.tree list*)
  
  (*raise NotImplemented*)
  

    
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