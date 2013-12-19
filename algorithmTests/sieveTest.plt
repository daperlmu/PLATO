main() {
  PRINT primes(100);
}
VECTOR<INTEGER> sieve(VECTOR<INTEGER> v, INTEGER n) {
  if (v[1] ^ 2 <= n) {
    RETURN ([v[1]] @ sieve(v[NOT (v % v[1] = 0)], n));
  } else {
    RETURN v;
  }
}
VECTOR<INTEGER> primes(INTEGER x) {
  RETURN sieve([2 TO x], x);
}
