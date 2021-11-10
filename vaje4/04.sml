(* Podan seznam xs agregira z začetno vrednostjo z in funkcijo f v vrednost f (f (f z s_1) s_2) s_3) ... *)
(* Aggregates xs with an initial value z and function f and returns f (f (f z s_1) s_2) s_3) ... *)
val reduce = fn : ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a

(* Vrne seznam, ki vsebuje kvadrate števil iz vhodnega seznama. Uporabite List.map. *)
(* Returns a list of squares of the numbers. Use List.map. *)
val squares = fn : int list -> int list

(* Vrne seznam, ki vsebuje vsa soda števila iz vhodnega seznama. Uporabite List.filter. *)
(* Returns a list that contains only even numbers from xs. Use List.filter. *)
val onlyEven = fn : int list -> int list

(* Vrne najboljši niz glede na funkcijo f (prvi arg.). Funkcija f primerja dva niza in vrne true, če je prvi niz boljši od drugega. Uporabite List.foldl. Najboljši niz v praznem seznamu je prazen niz. *)
(* Returns the best string according to the function f (first arg.). The function f compares two strings and returns true if the first string is better than the other. Use List.foldl. The best string in an empty list is an empty string. *)
val bestString = fn : (string * string -> bool) -> string list -> string

(* Vrne leksikografsko največji niz. Uporabite bestString. *)
(* Returns the largest string according to alphabetical ordering. Use bestString. *)
val largestString = fn : string list -> string

(* Vrne najdaljši niz. Uporabite bestString. *)
(* Returns the longest string. Use bestString. *)
val longestString = fn : string list -> string

(* Seznam uredi naraščajoče z algoritmom quicksort. Prvi argument je funkcija za primerjanje. *)
(* Sorts the list with quicksort. First argument is a compare function. *)
val quicksort = fn : ('a * 'a -> order) -> 'a list -> 'a list

(* Vrne skalarni produkt dveh vektorjev. Uporabite List.foldl in ListPair.map. *)
(* Returns the scalar product of two vectors. Use List.foldl and ListPair.map. *)
val dot = fn : int list -> int list -> int

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
val transpose = fn : 'a list list -> 'a list list

(* Zmnoži dve matriki. Uporabite dot in transpose. *)
(* Multiplies two matrices. Use dot and transpose. *)
val multiply = fn : int list list -> int list list -> int list list

(* V podanem seznamu prešteje zaporedne enake elemente in vrne seznam parov (vrednost, število ponovitev). Podobno deluje UNIX-ovo orodje
   uniq -c. *)
(* Counts successive equal elements and returns a list of pairs (value, count). The unix tool uniq -c works similarly. *)
val group = fn : ''a list -> (''a * int) list

(* Elemente iz podanega seznama razvrsti v ekvivalenčne razrede. Znotraj razredov naj bodo elementi v istem vrstnem redu kot v podanem seznamu. Ekvivalentnost elementov definira funkcija f, ki za dva elementa vrne true, če sta ekvivalentna. *)
(* Sorts the elements from a list into equivalence classes. The order of elements inside each equivalence class should be the same as in the original list. The equivalence relation is given with a function f, which returns true, if two elements are equivalent. *)
val equivalenceClasses = fn : ('a -> 'a -> bool) -> 'a list -> 'a list list