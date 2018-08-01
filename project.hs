data User = U String | I String deriving (Eq,Show) 
data Rating = R Float |NoRating deriving (Eq,Show)
dis :: Eq a => [a] -> [a]
dis [] = []
dis (x:xs)   | x `elem` xs   = dis xs
                | otherwise     = x : dis xs				   
				

fromRatingsToItems x = dis (fromRatingsToItems1 x)									 
fromRatingsToItems1 [] =[]
fromRatingsToItems1(( u , i , r):xs)= i:fromRatingsToItems1 xs  
								  
fromRatingsToUsers x = dis (fromRatingsToUsers1 x)									 
fromRatingsToUsers1 [] =[]
fromRatingsToUsers1(( u , i , r):xs)= u:fromRatingsToUsers1 xs									 
									 
hasRating _ _ []=False
hasRating y z ((a,b,c):xs) |y==a&&z==b = True
						   |otherwise =hasRating y z xs

getRating _ _ [] = error "No given Rating"
getRating y z ((a,b,c):xs)  |y==a&&z==b = c
				     		|otherwise = getRating y z xs
							
formMatrixUser x y z = formMatrixUser1 x y z z 
				
formMatrixUser1 x [] _	_ =[]
formMatrixUser1 x (y:ys) [] z = NoRating:formMatrixUser1 x ys z z 	
formMatrixUser1 x (y:ys) ((a,b,c):t) z|a==x&&b==y = R c:formMatrixUser1 x ys z z							
								      |otherwise = formMatrixUser1 x (y:ys) t z  

formMatrix [] _ _ = [] 
formMatrix (x:xs) y z = formMatrixUser x y z :formMatrix xs y z 

numberRatingsGivenItem _ [] = 0
numberRatingsGivenItem z (x:xs) = numberRatingsGivenItem2 z x 0+ numberRatingsGivenItem z xs 
numberRatingsGivenItem2 _ [] _ = 0
numberRatingsGivenItem2 z (x:xs) c |z==c &&x/=NoRating = 1
								   |z==c &&x==NoRating = 0 
								   |otherwise= numberRatingsGivenItem2 z xs (c+1)



--differeneRatings :: Fractional a => Rating a -> Rating a -> a
--diffreneRatings  r1 r2 | r1/=NoRating && r2/=NoRating =r1-r2
	--				   | r1==NoRating =0
		--			   | r2==NoRating =0
		
		
differeneRatings NoRating NoRating=0.0		
differeneRatings (R x)(R y)= x-y
differeneRatings NoRating (R y)= 0.0
differeneRatings (R x) NoRating= 0.0
--matrixPairs :: Num a => a -> [(a,a)]	
matrixPairs 0=[]
matrixPairs x=mphelper [0..x-1] [0..x-1] x
mphelper [] _ z=[]
mphelper (x:xs) [] z= mphelper xs [0..z-1] z
mphelper(x:xs) (y:ys) z= (x,y):mphelper (x:xs) ys z

{-dMatrix  (x:xs) = dhelper7 (matrixPairs(dhelper1 x) (x:xs)

dhelper7 y (x:xs) = dhelper8 y x

dhelper8 _ []=[]
dhelper8 ((a,b):ys)(x:xs)=  

dhelper1 [] = 0 
dhelper1 (x:xs)=1+dhelper1 xs
-}
 {-
dhelper2 _ [] = 0
dhelper2 z (x:xs) = dhelper3 z x 0: dhelper2 z xs 
dhelper3 _ [] _ = 0
dhelper3 z (x:xs) c |z==c &&x/=NoRating = x
								   |z==c &&x==NoRating = 0
								   |otherwise= dhelper3 z xs (c+1) 
d2 x |NoRating `elem` x = 0
	 |otherwise = d1 x
d1 []=0 
d1	(R x:xs) =x- d1 xs
-}		
		
		
		
		--2helpers 1 list so8ira 
		-- !!0 to get the first element 
		
		
dMatrix  (x:xs) = dMatrix3 (matrixPairs(dhelper1 x)) (x:xs)
dMatrix3 [] _ = []
dMatrix3 ((a,b):t) x= [dMatrix1 (a,b) x] ++dMatrix3 t x		
dMatrix1 _ []=0		
dMatrix1 (a,b) (x:xs)= dMatrix2 (a,b) x + dMatrix1 (a,b) xs

dMatrix2 (a,b)	x  =differeneRatings (x!!a) (x!!b)
dhelper1 [] = 0 
dhelper1 (x:xs)=1+dhelper1 xs


--dMatrix2 (a,b)	x  =x!!a - x!!b
		
		
		
freqMatrix  (x:xs) = freqMatrix3 (matrixPairs(dhelper1 x)) (x:xs)
freqMatrix3 [] _ = []
freqMatrix3 ((a,b):t) x= [freqMatrix1 (a,b) x] ++freqMatrix3 t x		
freqMatrix1 _ []=0		
freqMatrix1 (a,b) (x:xs)= freqMatrix2 (a,b) x + freqMatrix1 (a,b) xs

freqMatrix2 (a,b)	x  | x!!a/=NoRating && (x!!b)/=NoRating=1
						|otherwise =0
		
diffFreqMatrix x =diffFreqMatrix1 (dMatrix x) (freqMatrix x)	

diffFreqMatrix1 [] [] =[]
diffFreqMatrix1  (x:xs) (y:ys) = [x/y] ++ diffFreqMatrix1 xs ys
	
	
	
	
	
	
predict x y z = predict1(fromRatingsToUsers x) (fromRatingsToItems x) x y z



-- 
-- = predict8 
predict1 u i x y z	| hasRating (u!!y) (i!!z) x = getRating (u!!y) (i!!z) x
					|otherwise=predict8 (predict7 (u!!y) (predict6 [(u!!y)] i u i x y z) x (predict5 u i u i x y z) z) 

--predict4 to get the list that i will enter in dMAtrix					
{-predict4 [] _ _ _ _ _ _ =[]
predict4 (u:us) [] u1 i1 x y z = predict4 us i1 u1 i1 x y z 
predict4 (u:us) (i:is) u1 i1 x y z -- | u ==(u1!!y) = predict4 us i1 u1 i1 x y z
								   | otherwise = formMatrixUser u i1 x : predict4 us i1 u1 i1 x y z
-}
predict4 u i u1 i1 x y z = formMatrix u i x 								    					
predict5 u i u1 i1 x y z = diffFreqMatrix (predict4 u i u1 i1 x y z)
--row* 3adad al items + col
--to get the items user a rated
predict6 u [] u1 i1 x y z = []
predict6 u (i:is) u1 i1 x y z	| hasRating (u!!y) i x = i : predict6 u is u1 i1 x y z
								| otherwise = predict6 u is u1 i1 x y z 

--to get projecting
predict7 u [] x df z =[] 
predict7 u (i:is) x df z =(getRating u i x + 
 (df!!(z*(dhelper1 (fromRatingsToItems x))+((predict9 i (fromRatingsToItems x)))))
 ) :predict7 u is x df z							
--(df!!((predict9 i (fromRatingsToItems x))*(dhelper1 (fromRatingsToItems x))+(z)))
predict8 s = (sum s) /(dhelper1 s)  								

predict9 i [] = 0
predict9 i (i1:is) | i==i1 = 0
				   | otherwise = 1 + predict9 i is


--predict3 (u:us) (i:is) u1 i1 x y z = 				
predict2 _ [] u1 i1 x y z = 0 
--predict2 []	(i:is) u1 i1 x y z = predict2 u1 is u1 i1 x y z 
predict2 [] i u1 i1 x y z = 0
predict2 (u:us) (i:is) u1 i1 x y z | u ==(u1!!y) = predict2 us i1 u1 i1 x y z
								   | otherwise = (getRating u (i1!!z) x- predict2 (u:us) is u1 i1 x y z) 
								   
								   
								   
								   