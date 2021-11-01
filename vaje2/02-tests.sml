(* testi predvidevajo tudi implementacijo funkcije simp *)
val ena = Succ Zero;
val dve = Succ (Succ Zero);
val tri = Succ (Succ (Succ Zero));
val minus_ena = Pred Zero;
val minus_dve = Pred (Pred Zero);
val minus_tri = Pred (Pred (Pred Zero));
val ena_grdo = Pred (Pred (Succ (Pred (Succ (Succ (Succ Zero))))));
val minus_ena_grdo = Succ (Pred (Succ (Pred (Pred (Succ (Pred Zero))))));

(* testi tipov manjkajo *)
val testSimp1 = simp ena_grdo = ena;
val testSimp2 = simp minus_ena_grdo = minus_ena;
val testSimp3 = simp Zero = Zero;
val testSimp4 = simp ena = ena;

val testNeg1 = simp (neg ena) = simp minus_ena; (* pravilen odgovor je katerakoli (ne nujno ekonomična) reprezentacija negacije vhodnega argumenta *)
val testNeg1 = simp (neg ena) = minus_ena; (* implementacija neg naj ne bi uporabljala simp *)
val testNeg2 = simp (neg Zero) = Zero;
val testNeg3 = simp (neg minus_ena) = ena;
val testNeg4 = simp (neg ena_grdo) = minus_ena;
val testNeg5 = simp (neg minus_dve) = dve;

val testAdd1 = simp (add (ena, Zero)) = simp ena; (* pravilen odgovor je katerakoli (ne nujno ekonomična) reprezentacija vsote vhodnih argumentov *)
val testAdd1 = simp (add (ena, Zero)) = ena; (* implementacija  add naj ne bi uporabljala simp *)
val testAdd2 = simp (add (ena, minus_ena)) = Zero;
val testAdd3 = simp (add (minus_ena, ena)) = Zero;
val testAdd4 = simp (add (minus_ena, minus_dve)) = minus_tri;
val testAdd5 = simp (add (minus_dve, tri)) = ena;
val testAdd6 = simp (add (dve, minus_tri)) = minus_ena;
val testAdd7 = simp (add (ena, dve)) = tri;

val testComp1 = comp (Zero, Zero) = EQUAL;
val testComp2 = comp (Zero, ena) = LESS;
val testComp3 = comp (Zero, minus_ena) = GREATER;
val testComp4 = comp (minus_dve, Zero) = LESS;
val testComp5 = comp (minus_dve, minus_tri) = GREATER;
val testComp6 = comp (minus_tri, minus_ena) = LESS;
val testComp7 = comp (minus_ena, tri) = LESS;
val testComp8 = comp (tri, Zero) = GREATER;
val testComp9 = comp (minus_dve, minus_dve) = EQUAL;
val testComp10 = comp (ena_grdo, ena_grdo) = EQUAL;
val testComp11 = comp (ena_grdo, minus_ena_grdo) = GREATER;
val testComp12 = comp (tri, ena_grdo) = GREATER;
val testComp13 = comp (minus_ena_grdo, minus_ena) = EQUAL;
val testComp14 = comp (ena, ena_grdo) = EQUAL;


val tree1 = Node (2, Node (3, Leaf 1, Leaf 4), Node (5, Leaf 0, Leaf 6));
val tree2 = Leaf 1;
val tree3 = Node (2, Leaf 1, Node (3, Leaf 0, Leaf 0));
val tree4 = Node (2, Leaf 1, Leaf 4);
val tree5 = Node (8, Node (6, Node (2, Leaf 1, Node (4, Leaf 3, Leaf 5)), Leaf 7), Node (10, Leaf 9, Leaf 11));

val testContains1 = contains (tree1, 1) = true;
val testContains2 = contains (tree1, 2) = true;
val testContains3 = contains (tree1, 3) = true;
val testContains4 = contains (tree1, 4) = true;
val testContains5 = contains (tree1, 5) = true;
val testContains6 = contains (tree1, 6) = true;
val testContains7 = contains (tree1, 7) = false;
val testContains8 = contains (tree1, 8) = false;

val testHeight1 = height tree1 = 3;
val testHeight2 = height tree2 = 1;
val testHeight3 = height tree3 = 3;
val testHeight4 = height tree4 = 2;
val testHeight5 = height tree5 = 5;

val testToList1 = toList tree1 = [1, 3, 4, 2, 0, 5, 6]
val testToList2 = toList tree2 = [1]
val testToList3 = toList tree3 = [1, 2, 0, 3, 0]
val testToList4 = toList tree4 = [1, 2, 4]
val testToList5 = toList tree5 = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11]

val testIsBalanced1 = isBalanced tree1 = true;
val testIsBalanced2 = isBalanced tree2 = true;
val testIsBalanced3 = isBalanced tree3 = true;
val testIsBalanced4 = isBalanced tree4 = true;
val testIsBalanced5 = isBalanced tree5 = false;

val testIsBST1 = isBST tree1 = false;
val testIsBST2 = isBST tree2 = true;
val testIsBST3 = isBST tree3 = false;
val testIsBST4 = isBST tree4 = true;
val testIsBST5 = isBST tree5 = true;