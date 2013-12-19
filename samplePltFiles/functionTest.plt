main() {
  BOOLEAN x := TRUE;
  BOOLEAN y := x;
  y := y OR NOT y;
  INTEGER z1 := 1;
  NUMBER z2 := 2;
  PRINT x;
  PRINT y;
  PRINT z1;
  PRINT z2;  
  PRINT lol(6);
}
INTEGER lol(INTEGER a) {
  if(a>0) {
    RETURN -1;
  }
RETURN 0;
}
