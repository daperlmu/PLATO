main() {
  NUMBER OVER z4 x := 2;
  PRINT x + x;
  PRINT 2 + 2;
  PRINT x - x - x;
  PRINT 2 - 2 - 2;
}
GROUP z4 {
  {0, 1, 2};
  INTEGER add(INTEGER n1, INTEGER n2) {
    RETURN n1 * n2;
  }
}
