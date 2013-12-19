main() {
  NUMBER OVER z3 x := 2;
  PRINT x + x;
  PRINT 2 + 2;
  PRINT x - x - x;
  PRINT 2 - 2 - 2;
  NUMBER OVER z3plus1 y := 2;
  PRINT y + y;
}
GROUP z3 {
  {0, 1, 2};
  INTEGER add(INTEGER n1, INTEGER n2) {
    RETURN (n1 + n2) % 3;
  }
}
GROUP z3plus1 {
  {1, 2, 3};
  INTEGER add(INTEGER n1, INTEGER n2) {
    RETURN (n1 + n2 - 1) % 3;
  }
}
