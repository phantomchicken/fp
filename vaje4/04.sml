(* Podan seznam xs agregira z začetno vrednostjo z in funkcijo f v vrednost f (f (f z s_1) s_2) s_3) ... *)
(* Aggregates xs with an initial value z and function f and returns f (f (f z s_1) s_2) s_3) ... *)
(* val reduce = fn : ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a *)
fun reduce f z [] = z
  | reduce f z (x::xs) = reduce f (f z x) xs;
(* Vrne seznam, ki vsebuje kvadrate števil iz vhodnega seznama. Uporabite List.map. *)
(* Returns a list of squares of the numbers. Use List.map. *)
(* val squares = fn : int list -> int list *)
fun squares (s) = List.map (fn x => x * x) s;

(* Vrne seznam, ki vsebuje vsa soda števila iz vhodnega seznama. Uporabite List.filter. *)
(* Returns a list that contains only even numbers from xs. Use List.filter. *)
(* val onlyEven = fn : int list -> int list *)
fun onlyEven (s) = List.filter (fn x => x mod 2 = 0) s;

(* Vrne najboljši niz glede na funkcijo f (prvi arg.). Funkcija f primerja dva niza in vrne true, če je prvi niz boljši od drugega. Uporabite List.foldl. Najboljši niz v praznem seznamu je prazen niz. *)
(* Returns the best string according to the function f (first arg.). The function f compares two strings and returns true if the first string is better than the other. Use List.foldl. The best string in an empty list is an empty string. *)
(* val bestString = fn : (string * string -> bool) -> string list -> string *)

fun bestString f s = foldl (fn (x,y) => if f (x,y) then x else y) "" s;

(* Vrne leksikografsko največji niz. Uporabite bestString. *)
(* Returns the largest string according to alphabetical ordering. Use bestString. *)
(* val largestString = fn : string list -> string *)

(* Vrne najdaljši niz. Uporabite bestString. *)
(* Returns the longest string. Use bestString. *)
(* val longestString = fn : string list -> string *)

fun funkcijaLongest (x,y) = if size (x)>= size(y) then x else y;
fun funkcijaLargest (x,y) = 
      case String.compare(x, y) of
		 GREATER => true
		 | _ => false
(* fun longestString (s as g1::g2::r) = bestString (test(g1,g2) s); *)
(* fun longestString (s as x::y::r) = bestString (fn(x,y) => if size (x) >= size(y) then x else y) s;  *)
(* fun longestString (s as x::y::r) = bestString funkcija(x,y) s;  *)
fun largestString s = bestString funkcijaLargest s;
fun longestString s = foldl funkcijaLongest "" s;

(* Seznam uredi naraščajoče z algoritmom quicksort. Prvi argument je funkcija za primerjanje. *)
(* Sorts the list with quicksort. First argument is a compare function. *)
(* val quicksort = fn : ('a * 'a -> order) -> 'a list -> 'a list *)
fun quicksort _ x = x;

(* Vrne skalarni produkt dveh vektorjev. Uporabite List.foldl in ListPair.map. *)
(* Returns the scalar product of two vectors. Use List.foldl and ListPair.map. *)
(* val dot = fn : int list -> int list -> int *)

fun dot v1 v2 = foldl (fn (x,y) => x+y) 0 (ListPair.map(fn(x,y)=> x*y) (v1,v2));

(* Vrne transponirano matriko. Matrika je podana z vrstičnimi vektorji od zgoraj navzdol:
  [[1,2,3],[4,5,6],[7,8,9]] predstavlja matriko
   [ 1 2 3 ]
   [ 4 5 6 ]
   [ 7 8 9 ]
*)
(* Returns the transpose of m. The matrix m is given with row vectors from top to bottom:
  [[1,2,3],[4,5,6],[7,8,9]] represents the matrix
   [ 1 2 3 ]
   [ 4 5 6 ]
   [ 7 8 9 ]
*)
(* val transpose = fn : 'a list list -> 'a list list *)

fun getHeads [] = [] |
   getHeads [[],[]] = [] |
   getHeads [[],[],[]] = [] |
   (* getHeads [x] = x | *)
   getHeads s = [hd (hd s)] @ getHeads (tl(s));

fun removeHeads [] = [] |
   removeHeads [[],[]] = [[],[]] |
   removeHeads [[],[],[]] = [[],[],[]] |
   removeHeads s = [tl (hd s)] @ removeHeads (tl (s));


fun transpose [] = [] | 
   transpose [[]] = [[]] |
   transpose [[x]] = [[x]] |
   transpose [[],[]] = nil |
   transpose [[],[],[]] = nil |
   transpose s = [getHeads s] @ transpose (removeHeads s)

(* fun transposeWrap =  *)

(* fun transpose [[]] = [[]] |
   transpose [[x]] = [[x]] |
   transpose s = (let 
                  val r = removeHeads s
                 in 
                  ([getHeads s] @ [transpose r])
                 end); *)

(* transpose [ [1,2], [3,4] ] = [ [1, 3], [2,4] ]; *)
(* val test2 = transpose [ [1,2,3],[4,5,6],[7,8,9] ] = [ [1,4,7], [2,5,8], [3,6,9] ]; *)

(* Zmnoži dve matriki. Uporabite dot in transpose. *)
(* Multiplies two matrices. Use dot and transpose. *)
(* val multiply = fn : int list list -> int list list -> int list list *)
fun multiply m1 m2 = m1;

(* fun multiplyHelper m1 m2t = 
   case m1 of 
      [] => []
      | (g::r) => [List.map(fn (x) => dot (g,x)) m2t] @ (multiplyHelper(r, m2t))


fun multiply m1 m2 = 
   let 
      val m2t = transpose m2
   in
      multiplyHelper m1 m2t
      (* List.map (fn x => x * x) s;
      List
      dot prvi drugi *)
   end *)


(* V podanem seznamu prešteje zaporedne enake elemente in vrne seznam parov (vrednost, število ponovitev). Podobno deluje UNIX-ovo orodje
   uniq -c. *)
(* Counts successive equal elements and returns a list of pairs (value, count). The unix tool uniq -c works similarly. *)
(* val group = fn : ''a list -> (''a * int) list *)

(* fun isolate [] = []
  | isolate (l as x::xs) =
      let fun remove (x,[]) = []
            | remove (x,l as y::ys) = if x = y
                                      then remove(x,ys)
                                      else y::remove(x,ys)
      in
        x::isolate(remove(x,xs))
      end *)

fun isolate' [] = []
  | isolate' (x::nil) = x::isolate'(nil)
  | isolate' (x::y::xs) = if x=y then isolate'(y::xs) else x::isolate'(y::xs);

fun countSame ([],countlist,counter) = countlist @ [counter+1] |
   countSame (l as x1::x2::xs, countlist, counter) = 
      if x1=x2
      then countSame(x2::xs, countlist, counter+1) 
      else countSame(x2::xs, countlist @ [counter], 1) | 
   countSame (l as x1::nil,countlist,counter) = countlist @ [counter]

fun group l = ListPair.zip (isolate' l , countSame (l, [], 1));

(* Elemente iz podanega seznama razvrsti v ekvivalenčne razrede. Znotraj razredov naj bodo elementi v istem vrstnem redu kot v podanem seznamu. Ekvivalentnost elementov definira funkcija f, ki za dva elementa vrne true, če sta ekvivalentna. *)
(* Sorts the elements from a list into equivalence classes. The order of elements inside each equivalence class should be the same as in the original list. The equivalence relation is given with a function f, which returns true, if two elements are equivalent. *)
(* val equivalenceClasses = fn : ('a -> 'a -> bool) -> 'a list -> 'a list list *)
(* fun equivalenceClasses f s = 
   	case s of 
      [] => []
      | g::r => (let val a = List.partition (fn x => f g x) s
                in 
                end) *)

fun equivalenceClasses f xs =
	case xs of
		nil => nil
		| h::t => let
				val (pos, neg) = List.partition(fn x => f h x) t
				val rest = (equivalenceClasses f)(neg)
			in
				(h::pos)::rest
			end
;