/*
{x} and {y} are of different types, so we shouldn't be able to union set {x} with set {y}
*/
main() {
  NUMBER OVER GROUP z3 x := 2;
  NUMBER OVER GROUP z2 y := 1;
  PRINT {x}+{y};
}
GROUP z2 {
  SET<INTEGER> elements := {0, 1};
  INTEGER add(INTEGER n1, INTEGER n2) {
    RETURN (n1 + n2) % 2;
  }
}
GROUP z3 {
  SET<INTEGER> elements := {0, 1, 2};
  INTEGER add(INTEGER n1, INTEGER n2) {
    RETURN (n1 + n2) % 3;
  }
}
