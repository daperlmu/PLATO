main() {

	BOOLEAN x := TRUE;
 
  NUMBER z2 := 2;
  PRINT x;
  PRINT functionWithReturnType();
  PRINT functionWithBooleanReturn();
	
}
INTEGER functionWithReturnType() {
	BOOLEAN y := FALSE;
  y := y OR NOT y;
  INTEGER z1 := 1;
  RETURN -1;
}
VOID voidFunction() {
	PRINT 3;
	PRINT 42;
}
BOOLEAN functionWithBooleanReturn() {
	RETURN TRUE;
}