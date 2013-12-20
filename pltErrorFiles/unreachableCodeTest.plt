main() {

	BOOLEAN r := TRUE;
 
  NUMBER z2 := 2;
  PRINT functionWithIfElse(TRUE);
  PRINT functionWithBooleanReturn();
	
}
INTEGER functionWithIfElse(BOOLEAN x) {
  if(x) {
    RETURN -1;
  }
  elseif(3 > 0) {
    RETURN 4;
  }
  else {
    RETURN 0;
  }
	RETURN -2;
}
BOOLEAN functionWithBooleanReturn() {
	RETURN TRUE;
}