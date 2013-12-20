main() {

	BOOLEAN r := TRUE;
 
  NUMBER z2 := 2;
  PRINT functionWithIfElse(TRUE);
  PRINT functionWithBooleanReturn();
	
}
INTEGER functionWithIfElse(BOOLEAN x) {
	BOOLEAN y := x;
  y := y OR NOT y;
  INTEGER z1 := 1;
  PRINT x;
  if(z1 > 3){
  	PRINT 0;
  }
  elseif (y){
  	RETURN 42;
  }
}
BOOLEAN functionWithBooleanReturn() {
	RETURN TRUE;
}