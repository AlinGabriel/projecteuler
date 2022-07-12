-- @author Alin Gabriel Arhip


-- Problem 1 Multiples of 3 or 5
multiples :: Int
multiples = sum [ x | x <- [3..999], x `mod` 3 == 0 || x `mod` 5 == 0]

-- (multiples) == 233168


-- Problem 2 Even Fibonacci numbers 
fib :: Integral a => a -> a
fib n = fibo 1 1 n
  where fibo a _ 1 = a
        fibo a b count = fibo (a + b) a (count - 1)

fibsum :: Integral a => a -> a
fibsum limit = sum (takeWhile (< limit) [ fib n | n <- [2,5..]])

-- (fibsum 4000000) == 4613732


-- Problem 3 Largest prime factor
maxprimeof :: Integral a => a -> a
maxprimeof x = primefactors x 2 1
  where primefactors n d maxprime
          | q < 1 = maxprime
          | n `mod` d == 0 = primefactors q d d
          | otherwise = primefactors n (d + 1) maxprime
          where q = n `quot` d
 
-- (maxprimeof 600851475143) == 6857


-- Problem 4 Largest palindrome product
maxpalindrome :: (Integral a, Show a) => a
maxpalindrome = head [p | x <- [999,997..901], y <- [x,x-2..901], 
  let p = y * x, let mirror = show p, mirror == reverse mirror]

-- (maxpalindrome) == 906609


-- Problem 5 Smallest multiple
minmultiple :: Integral a => a
minmultiple = product [5,7,9,11,13,16,17,19]

-- (minmultiple == 232792560)


-- Problem 6 Sum square difference
diff :: Integral a => a -> a
diff n = squaredsum - sumofsquares
  where squaredsum = ((n * (n + 1)) ^ 2) `div` 4
        sumofsquares = (n * (n + 1) * ( 2 * n + 1)) `div` 6

-- (diff 100) == 25164150


-- Problem 7 10001st prime
isprime :: Integral a => a -> Bool
isprime 2 = True
isprime n
  | even n = False
  | otherwise = null [d | d <- [3, 5..(floor (sqrt (fromIntegral n)))], n `mod` d == 0]

nthprime :: Integral a => a -> a
nthprime 1 = 2
nthprime n = findme 3 2
  where findme x count
          | isprime x = if (n == count) then x else findme (x + 2) (count + 1)
          | otherwise = findme (x + 2) count

-- (nthprime 10001) == 104743


-- Problem 8 Largest product in a series
bignumber :: Integer 
bignumber = read $  "73167176531330624919225119674426574742355349194934" ++ 
                    "96983520312774506326239578318016984801869478851843" ++ 
                    "85861560789112949495459501737958331952853208805511" ++
                    "12540698747158523863050715693290963295227443043557" ++
                    "66896648950445244523161731856403098711121722383113" ++
                    "62229893423380308135336276614282806444486645238749" ++
                    "30358907296290491560440772390713810515859307960866" ++
                    "70172427121883998797908792274921901699720888093776" ++
                    "65727333001053367881220235421809751254540594752243" ++
                    "52584907711670556013604839586446706324415722155397" ++
                    "53697817977846174064955149290862569321978468622482" ++
                    "83972241375657056057490261407972968652414535100474" ++
                    "82166370484403199890008895243450658541227588666881" ++
                    "16427171479924442928230863465674813919123162824586" ++
                    "17866458359124566529476545682848912883142607690042" ++
                    "24219022671055626321111109370544217506941658960408" ++
                    "07198403850962455444362981230987879927244284909188" ++
                    "84580156166097919133875499200524063689912560717606" ++
                    "05886116467109405077541002256983155200055935729725" ++
                    "71636269561882670428252483600823257530420752963450"

digits :: Integer -> [Int]
digits n = map (\x -> read [x] :: Int ) (show n)

bigproduct :: Integer -> Int -> Int
bigproduct n x = findmax nums (take x nums) 1
  where nums = digits n
        findmax [] _ maxp = maxp
        findmax xs ys maxp
          | 0 `elem` ys = let zs = tail (dropWhile (0 /=) xs)
                          in findmax zs (take x zs) maxp
          | otherwise = let zs = tail xs
                            posnewmax = product ys
                            newmax = if (posnewmax > maxp) then posnewmax else maxp
                        in findmax zs (take x zs) newmax

-- (bigproduct bignumber 13) == 23514624000


-- Problem 9 Special Pythagorean triplet
triplet :: Integral a => a
triplet = head [a * b * c | a <- [3..1000], b <- [(a+1)..(1000-a-1)], 
  let c = 1000 - a - b , a ^ 2 + b ^ 2 == c ^ 2]

-- (triplet) == 31875000


-- Problem 10 Summation of primes
sieve :: Integral a => a -> a
sieve n = sieverec 2 [3,5..n] 
  where rootofn = floor (sqrt (fromIntegral n)) 
        sieverec acc ys@(x:xs)
          | x > rootofn = acc + sum ys
          | otherwise = sieverec (x + acc) (primes ++ [y | y <- notprimes, y `mod` x > 0]) 
          where primes = takeWhile (< x ^ 2) xs
                notprimes = dropWhile (< x ^ 2) xs

-- (sieve 2000000) == 142913828922

