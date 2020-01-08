half x = x / 2

double x = x + x

sieve (p:ns) = p:sieve [n| n <- ns, n `mod` p /= 0]
primes = sieve [2..]

nthprime n = primes!!(n-1)

half' x = x `div` 2