main() {
  VECTOR<INTEGER> myVector := [1 TO 5 BY 2];
  myVector[1] := 9000;
  myVector[2] := myVector[1]^2;
  PRINT myVector;
}
