
import Control.Monad.State


tick :: State Int Int
tick = do n <- get
          put (n+1)
          return n

-- Add one to the given number using the state monad:

plusOne :: Int -> Int
plusOne n = execState tick n

-- A contrived addition example. Works only with positive numbers:

plus :: Int -> Int -> Int
plus n x = execState (sequence $ replicate n tick) x

inc :: State Int Int
inc = do
    n <- get
    put (n + 1)
    return n

incBy :: Int -> State Int Int
incBy x = do
    n <- get
    modify (+x)
    return n

main2 = do
    print $ evalState inc 1
    print $ execState inc 1
    print $ runState inc 1
    print $ runState (withState (+3) inc) 1
    print $ runState (mapState (\(a, s) -> (a + 3, s + 4)) inc) 1

    print $ runState (incBy 5) 10

test :: State Int Int
test = do
  put 3
  modify (+1)
  get

main :: IO ()
main = print $ execState test 0

