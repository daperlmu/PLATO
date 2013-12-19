main() {
  VECTOR<INTEGER> myVector := [1 TO 5 BY 2];
  VECTOR<INTEGER> myVector2 := myVector[[TRUE, FALSE, TRUE]];
  PRINT myVector2;
}
