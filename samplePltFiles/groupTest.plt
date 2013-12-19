main() {
  NUMBER OVER GROUP z3 x := 2;
  PRINT x + x;
  PRINT 2 + 2;
  PRINT x - x - x;
  PRINT 2 - 2 - 2;
  NUMBER OVER GROUP z3plus1 y := 2;
  PRINT y + y;
}
GROUP z3 {
  SET<INTEGER> elements := {0, 1, 2};
  INTEGER add(INTEGER n1, INTEGER n2) {
    RETURN (n1 + n2) % 3;
  }
}
GROUP z3plus1 {
  SET<INTEGER> elements := {1, 2, 3};
  INTEGER add(INTEGER n1, INTEGER n2) {
    RETURN ((n1 - 1) + (n2 - 1)) % 3 + 1;
  }
}
