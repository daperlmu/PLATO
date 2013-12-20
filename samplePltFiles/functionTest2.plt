main() {

	BOOLEAN x := TRUE;
 
  NUMBER z2 := 2;
  PRINT x;
	PRINT functionwithreturntype();
}
INTEGER functionwithreturntype() {
	BOOLEAN y := FALSE;
  y := y OR NOT y;
  INTEGER z1 := 1;
  RETURN -1;
}