main() {
  NUMBER OVER RING z3 x := 2;
  PRINT x / x;
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
