(* Vrne rezultat potenciranja - base ^ exponent (exponent > 0)
   pow (5, 3) = 125 *)
fun pow (base : LargeInt.int, exponent : LargeInt.int) : LargeInt.int = 
    if exponent = 1 then base
    else if exponent = 2 then base*base
    else if exponent mod 2 = 0 then pow(pow (base, exponent div 2),2) (*kvadrira rez. funkcije*)
    else base * pow(pow (base, exponent div 2),2);

(* Vrne rezultat logaritmiranja - ceilling(log_base(n))  (n > 0, base > 1)
   log (3, 12345) = 8 *)
fun log (base : int, n : int) : int = 
    if n < 1 then 0
    else 1 + log (base, n div base)

(* Vrne Hammingovo razdaljo med binarnimi reprezentacijami dveh naravnih števil x in y
   10 =  1010
   6  =  0110
   hammingDistance (10, 6) = 2 *)
fun hammingDistance (x : int, y : int) : int =
    if x = y then 0
    else (x+y) mod 2 + hammingDistance(x div 2, y div 2) (* seštej po mod 2 = 1 ko se razlikujeta v zadnjem bitu, rekurzivno pokliči naprej na ostanku (deli z 2)*);