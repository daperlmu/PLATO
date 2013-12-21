main() {
  INTEGER a := 4;
  INTEGER b := -100000;
  PRINT cases(a, b);
  PRINT cases(b, a);
}
INTEGER cases(INTEGER x, INTEGER y) {
  RETURN {
    2           IF 3<2;
    100         IF x<y;
    42          OTHERWISE
  };
}
