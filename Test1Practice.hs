module Test1Practice where

	sortedPrefix :: [Int] -> [Int]
	sortedPrefix [] = []
	sortedPrefix [a] = [a]
	sortedPrefix (x:xs)
		| x <= head(xs) = x:(sortedPrefix xs)
		| otherwise = [x]


	characterCount :: Char -> [Char] -> Int
	characterCount c list = length [t | t<-list, t == c]


	isHarshad :: Int -> Bool
	isHarshad x
		| mod x (sumDigits x) == 0 = True
		| otherwise = False
	sumDigits :: Int -> Int
	sumDigits x
		| x < 10 = x
		| otherwise = mod x 10 + sumDigits (div x 10)


	oneBubblePass :: [Int] -> [Int]
	oneBubblePass [] = []
	oneBubblePass [a] = [a]
	oneBubblePass (x:xs)
		| x > head(xs) = head(xs):(oneBubblePass (x:tail(xs)))
		| otherwise = x:(oneBubblePass xs)


	sumTuple :: [(Float, Float)] -> Float
	sumTuple [] = 0
	sumTuple list = sum [x+y | (x, y) <- list]


	alternate :: Int -> [a] -> [a]
	alternate n (x:xs)
		| n < 1 = error "Error: function called with n < 1"
		| otherwise = newAlternate n (x:xs) 1
	newAlternate :: Int -> [a] -> Int -> [a]
	newAlternate n (x:xs) counter
		| length (xs) == 0 && mod counter n == 0 = [x]
		| length (xs) == 0 = []
		| mod counter n == 0 = x:(newAlternate n xs (counter+1))
		| otherwise = newAlternate n xs (counter+1)
