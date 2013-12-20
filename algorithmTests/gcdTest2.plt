main() {
  PRINT gcd(18, 12);
}
INTEGER gcd(INTEGER a, INTEGER b) {
  RETURN { a              IF b=0;
           gcd(b, a % b)  OTHERWISE };
}
