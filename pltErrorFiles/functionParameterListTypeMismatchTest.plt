main() {

	BOOLEAN r := TRUE;
 
  NUMBER z2 := 2;
  PRINT functionWithIfElse(TRUE, 0, FALSE, 9);
  PRINT functionWithBooleanReturn();
	
}
INTEGER functionWithIfElse(BOOLEAN x, INTEGER a, INTEGER b, BOOLEAN c) {
  if(x) {
    RETURN -1;
  }
  elseif(3 > 0) {
    RETURN 4;
  }
  else {
    RETURN 0;
  }
}
BOOLEAN functionWithBooleanReturn() {
	RETURN TRUE;
}