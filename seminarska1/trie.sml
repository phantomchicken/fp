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
  fun insertHelper [] dict = []
        | insertHelper (g::nil) dict = dict @ [N(g,true,insertHelper [] dict)]
        | insertHelper (g::r) dict = dict @ [N(g,false,insertHelper r dict)]
  
  fun insertHelper' [] dict = []
  | insertHelper' (x::xs) (y::ys) = ys  


  (*foldl (fn(el,acc) => el::acc) empty w*)

  fun insert w dict =
    if dict = empty then (insertHelper w dict)
    else insertHelper' w dict

    
    
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