main() {
  NUMBER OVER GROUP z3 x := 2;
  PRINT x + x;
  PRINT 2 + 2;
  PRINT x - x - x;
  PRINT 2 - 2 - 2;
}
GROUP z3 {
  SET<INTEGER> elements := {0, 1, 2};
  INTEGER add(INTEGER n1, INTEGER n2) {
    RETURN n1 + n2;
  }
}
