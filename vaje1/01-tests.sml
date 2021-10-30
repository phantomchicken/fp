val _ : int -> int = factorial;
val testFactorial1 = factorial (0) = 1;
val testFactorial2 = factorial (1) = 1;
val testFactorial3 = factorial (2) = 2;
val testFactorial4 = factorial (3) = 6;
val testFactorial5 = factorial (4) = 24;

val _ : int*int -> int = power;
val testPower1 = power (0, 10) = 0;
val testPower2 = power (1, 10) = 1;
val testPower3 = power (2, 2) = 4;
val testPower4 = power (3, 3) = 27;
val testPower5 = power (3, 2) = 9;
val testPower6 = power (4, 2) = 16;
val testPower7 = power (6, 2) = 36;
val testPower8 = power (10, 0) = 1;
val testPower9 = power (10, 1) = 10;

val _ : int*int -> int = gcd;
val testGcd1 = gcd (1, 1) = 1;
val testGcd2 = gcd (10, 0) = 10;
val testGcd3 = gcd (10, 1) = 1;
val testGcd4 = gcd (8, 2) = 2;
val testGcd5 = gcd (16, 8) = 8;
val testGcd6 = gcd (8, 4) = 4;
val testGcd7 = gcd (16, 32) = 16;
val testGcd8 = gcd (8, 4) = 4;
val testGcd9 = gcd (36, 16) = 4;

val _ : int list -> int = len;
val testLen1 = len ([]) = 0;
val testLen2 = len ([1, 2]) = 2;
val testLen3 = len ([3, 3, 3]) = 3;
val testLen4  = len([1, 2, 4, 5, 6, 7]) = 6;
val testLen5 = len([1, 2, 4, 5, 6, 7, 5, 4, 6, 7]) = 10;
val testLen6 = len([1]) = 1;

val _: (int list * int * int) -> int list = insert;
val testInsert1 = insert ([], 0, 5) = [5];
val testInsert2 = insert ([1, 2], 0, 5) = [5, 1, 2];
val testInsert3 = insert ([1, 2], 1, 5) = [1, 5, 2];
val testInsert4 = insert ([1, 2, 3], 1, 5) = [1, 5, 2, 3];
val testInsert5 = insert([1, 2, 4, 5, 6, 7], 3, 99) = [1, 2, 4, 99, 5, 6, 7];
val testInsert6 = insert([1, 2, 4, 5, 6, 7, 5, 4, 6, 7, 456], 5, 99) = [1, 2, 4, 5, 6, 99, 7, 5, 4, 6, 7, 456];
val testInsert7 = insert([1], 1, 2) = [1, 2];

val _: int list -> int option = last;
val testLast1 = last([1, 2, 4, 5, 6, 7]) = SOME (7);
val testLast2 = last([1, 2, 4, 5, 6, 7, 5, 4, 6, 7, 456]) = SOME (456);
val testLast3 = last([1]) = SOME (1);
val testLast4 = last(nil) = NONE;

val _: (int list * int) -> int option = nth;
val testNth1 = nth([1, 2, 4, 5, 6, 7], 3) = SOME 5;
val testNth2 = nth([1, 2, 4, 5, 6, 7, 5, 4, 6, 7, 456], 5) = SOME 7;
val testNth3 = nth([1], 0) = SOME 1;
val testNth4 = nth([1], 1) = NONE;
val testNth5 = nth([1, 2], 2) = NONE;

val _ : int list*int -> int list = delete;
val testDelete1 = delete ([], 0) = [];
val testDelete2 = delete ([1, 2], 0) = [1,2];
val testDelete3 = delete ([1, 2], 1) = [2];
val testDelete4 = delete ([1, 2, 3, 3, 3, 3], 3) = [1, 2];
val testDelete5 = delete ([1, 2, 3, 2, 4, 2, 5, 2], 2) = [1, 3, 4, 5];
val testDelete6 = delete([1, 2, 4, 5, 6, 7], 4) = [1, 2, 5, 6, 7];
val testDelete7 = delete([1, 2, 4, 5, 6, 7, 5, 4, 6, 5], 5) = [1, 2, 4, 6, 7, 4, 6];
val testDelete8 = delete([1], 1) = [];

val _ : int list*int -> int list = append;
val testAppend1 = append ([], 0) = [0];
val testAppend2 = append ([1,2], 0) = [1,2,0];
val testAppend3 = append ([1,2], 1) = [1,2,1];
val testAppend4 = append ([1,2,3,3,3,3], 3) = [1,2,3,3,3,3,3];
val testAppend5 = append ([1,2,3,2,4,2,5,2], 2) = [1,2,3,2,4,2,5,2,2];

val _ : int list -> int list = reverse;
val testReverse1 = reverse ([]) = [];
val testReverse2 = reverse ([1]) = [1];
val testReverse3 = reverse ([1,2]) = [2,1];
val testReverse4 = reverse ([1,2,3]) = [3,2,1];
val testReverse5 = reverse ([1,2,3,3]) = [3,3,2,1];
val testReverse6 = reverse([1, 2, 4, 5, 6, 7]) = [7, 6, 5, 4, 2, 1];
val testReverse7 = reverse([1, 2, 4, 5, 6, 7, 5, 4, 6, 5]) = [5, 6, 4, 5, 7, 6, 5, 4, 2, 1];
val testReverse8 = reverse([1]) = [1];

val _ : int list -> bool = palindrome;
val testPalindrom1 = palindrome ([]) = true;
val testPalindrom2 = palindrome ([1]) = true;
val testPalindrom3 = palindrome ([1, 2]) = false;
val testPalindrom4 = palindrome ([1, 2, 3]) = false;
val testPalindrom5 = palindrome ([1, 2, 2, 1]) = true;
val testPalindrom6 = palindrome ([1, 2, 1]) = true;
val testPalindrom7 = palindrome ([1, 2, 2, 0]) = false;
val testPalindrom8 = palindrome ([1, 1]) = true;
val testPalindrom9 = palindrome([1, 2, 4, 4, 2, 1]) = true;
val testPalindrom10 = palindrome([1, 2, 4, 5, 4, 2, 1]) = true;