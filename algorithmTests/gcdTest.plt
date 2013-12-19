main() {
  PRINT gcd(18, 12);
}
INTEGER gcd(INTEGER a, INTEGER b) {
  if (b = 0) {
    RETURN a;
  } else {
    RETURN gcd(b, a % b);
  }
}
