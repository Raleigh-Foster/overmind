module Main

import Data.Vect




{- don't forget about bias -}
{-depth : Nat-}
{-widths : Vect (depth + 1) Nat-}

data NeuralNetwork : Nat -> Nat -> Type where
 Synthesize : Vect (S o) (Vect (S i) Double) -> NeuralNetwork (S i) (S o)
 Fuse : NeuralNetwork (S a) (S b) -> NeuralNetwork (S b) (S c) -> NeuralNetwork (S a) (S c)
 Connect : NeuralNetwork (S a) (S b) -> NeuralNetwork (S c) (S d) -> Vect (S c) (Vect (S b) Double) -> NeuralNetwork (S a) (S d)




{-ignoring bias for now.-}



dot : Vect (S n) Double -> Vect (S n) Double -> Double

{- dot a b = foldr1 (+) . zipWith (*) a $ b-}
dot = ((foldr1 (+)) .) . zipWith (*)

transform : Vect (S o) (Vect (S i) Double) -> Vect (S i) Double -> Vect (S o) Double
transform weights input = map (dot input) weights


sigmoidActivation : Double -> Double -> Double
sigmoidActivation c x = 1 / (1 + exp (-c*x))



activate : Vect (S n) Double -> Vect (S n) Double
activate activities = map (sigmoidActivation 1) activities


{-activation function is the identity for now!-}

compute : NeuralNetwork i o -> Vect i Double -> Vect o Double
compute (Synthesize weights) input = activate $ transform weights input
compute (Fuse _ _) input = ?hole
compute (Connect front back weights) input =
 let input' = compute front input in
 let input'' = transform weights input' in
 compute back input''

viewOutput : Vect (S n) Double -> String
viewOutput output = foldr1 (++) . map (++ ";") $ map show output



testWeights : Vect 2 (Vect 2 Double)
testWeights = [[1,1],[1,1]]

testNetwork : NeuralNetwork 2 2
testNetwork = Synthesize testWeights

testInput : Vect 2 Double
testInput = [1,1]

testOutput : Vect 2 Double
testOutput = compute testNetwork testInput



singleSquaredError : Double -> Double -> Double
singleSquaredError output target =
 let err = output - target in
 err*err

squaredError : Vect (S n) Double -> Vect (S n) Double -> Double
squaredError = ((foldr1 (+)).) . zipWith singleSquaredError

correctOutput : Vect 2 Double
correctOutput = [10,6]





main : IO ()
main = do putStrLn $ "input : " ++ (show testInput)
          putStrLn $ "output : " ++ (show testOutput)
          putStrLn $ "target output : " ++ (show correctOutput)
          putStrLn $ "total squared error : " ++ (show $ squaredError testOutput correctOutput)
          putStrLn $ "detailed squared error : " ++ (show $ zipWith singleSquaredError testOutput correctOutput)


















