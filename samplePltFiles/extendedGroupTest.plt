main() {
  NUMBER OVER z3 x := 2;
  PRINT x * x;
  PRINT 2 * 2;
  NUMBER OVER z3 y := 2;
  PRINT y / 3;
  PRINT 2 / 3;
}
RING z3 {
  {0, 1, 2};
  INTEGER add(INTEGER n1, INTEGER n2) {
    RETURN (n1 + n2) % 3;
  }
  INTEGER multiply(INTEGER n1, INTEGER n2) {
    RETURN (n1 * n2) % 3;
  }
}
FIELD z5 {
  {0, 1, 2, 3, 4};
  INTEGER add(INTEGER n1, INTEGER n2) {
    RETURN (n1 + n2) % 5;
  }
  INTEGER multiply(INTEGER n1, INTEGER n2) {
    RETURN (n1 * n2) % 5;
  }
}
