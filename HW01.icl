module HOMEW1
import StdEnv

//Digit to 

digit_to_string :: Int -> String
digit_to_string num
| num == 0 = "Zero"
| num == 1 = "One"
| num == 2 = "Two"
| num == 3 = "Three"
| num == 4 = "Four"
| num == 5 = "Five"
| num == 6 = "Six"
| num == 7 = "Seven"
| num == 8 = "Eight"
| num == 9 = "Nine"
="Not a digit"

//Start = digit_to_string 4 //"Four"
//Start = digit_to_string 0 //"Zero"
//Start = digit_to_string 5 //"Five"
//Start = digit_to_string 10 //"Not a digit"
//Start = digit_to_string -1 //"Not a digit"
//Start = digit_to_string 42 //"Not a digit"

//Prime check

is_prime :: Int -> Bool
is_prime 2 = True
is_prime 1 = False
is_prime 0 = False
is_prime -1 = False
is_prime -2 = True
is_prime n
|n > 0 = posit 2 n
|n < 0 = neg -2 n

posit :: Int Int -> Bool
posit i n 
| i == n-1 = True
= div (i) (n) && posit (i+1) (n)

div :: Int Int -> Bool
div d1 d2 = d2 rem d1 <> 0 

neg :: Int Int -> Bool
neg i n
|i == n+1 = True
=div (i) (n) && neg (i-10) (n) 

//Start is_prime 5 // True
//Start is_prime 0 // False
// Start is_prime 1 // False
// Start is_prime 2 // True
// Start is_prime 2017 // True

//Palindorm check

N2S :: Int -> [Int]
N2S 0 = []
N2S x = [x rem 10] ++ N2S (x/10)

is_palindrome :: Int -> Bool
is_palindrome n = tst (N2S n)

tst :: [Int] -> Bool
tst num = num == reverse num 

Start = is_palindrome 0 // True
// Start = is_palindrome 55 // True
// Start = is_palindrome 49594 // True
// Start = is_palindrome 1337 // False