main() {
  PRINT primes(100);
}
VECTOR<INTEGER> sieve(VECTOR<INTEGER> v, INTEGER n) {
  RETURN {
    [v[1]]@sieve(v[NOT(v%v[1]=0)],n)           IF v[1]^2 <= n;
    v                                          OTHERWISE
  };
}
VECTOR<INTEGER> primes(INTEGER x) {
  RETURN sieve([2 TO x], x);
}
