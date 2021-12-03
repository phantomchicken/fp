val _ = Control.Print.printDepth := 100;
val _ = Control.Print.printLength := 100;
val _ = Control.Print.stringDepth := 2000;
val _ = Control.polyEqWarn := false;

exception NotImplemented;
structure Trie =
struct
  datatype ''a tree = N of ''a * bool * ''a tree list
  type ''a dict = ''a tree list

  val empty = [] : ''a dict;

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

    fun insert word dict = 
    case (word,dict) of 
          ([w], N(crka, jeKonec, rep)::rep2) => if w=crka then N(crka, true, rep)::rep2 else N(crka, false, rep)::rep2
          | (w::ws, []) => makeDict (w::ws)
          | (w::ws, N(crka, jeKonec, rep)::rep2) => if w = crka then [N(crka, jeKonec, (insert ws rep))] @ rep2 else [N(crka, jeKonec, rep)] @ (insert word rep2)
    
end

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