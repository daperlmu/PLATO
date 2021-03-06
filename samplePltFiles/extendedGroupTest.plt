main() {
  NUMBER OVER RING z3 x := 2;
  PRINT x * x;
  PRINT 2 * 2;
  NUMBER OVER FIELD z5 y := 2;
  NUMBER OVER FIELD z5 y2 := 3;
  PRINT y / y2;
  PRINT 2 / 3;
}
RING z3 {
  SET<INTEGER> elements := {0, 1, 2};
  INTEGER add(INTEGER n1, INTEGER n2) {
    RETURN (n1 + n2) % 3;
  }
  INTEGER multiply(INTEGER n1, INTEGER n2) {
    RETURN (n1 * n2) % 3;
  }
}
FIELD z5 {
  SET<INTEGER> elements := {0, 1, 2, 3, 4};
  INTEGER add(INTEGER n1, INTEGER n2) {
    RETURN (n1 + n2) % 5;
  }
  INTEGER multiply(INTEGER n1, INTEGER n2) {
    RETURN (n1 * n2) % 5;
  }
}
