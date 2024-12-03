-- this is just me experimenting with monads following along the Computerphile video on the subject
data Expr = Val Int | Div Expr Expr

-- this eval is not safe as dividing by 0 is undefined
brokenEval :: Expr -> Int
brokenEval (Val n) = n
brokenEval (Div x y) = brokenEval x `div` brokenEval y

-- safe division
safeDiv :: Int -> Int -> Maybe Int
safeDiv x y
  | y == 0 = Nothing
  | otherwise = Just (x `div` y)

-- this eval is safe I believe
eval :: Expr -> Maybe Int
eval (Val n) = Just n
eval (Div x y)
  | Just v <- eval x, Just w <- eval y = safeDiv v w
  | otherwise = Nothing


eval2 :: Expr -> Maybe Int
eval2 (Val n) = return n
eval2 (Div x y) = eval x >>= (\n ->                 -- after unwrapping x
                  eval y >>= (\m -> safeDiv n m))   -- after unwrapping y, safeDiv


eval3 :: Expr -> Maybe Int
eval3 (Val n) = return n
eval3 (Div x y) = do n <- eval x                    -- after unwrapping x
                     m <- eval y                    -- after unwrapping y
                     safeDiv n m                    -- safeDiv

-- Here I'm done with the computerphile video on monads, but I'm still a little confused by list monads so currently following:
-- https://www.schoolofhaskell.com/school/starting-with-haskell/basics-of-haskell/13-the-list-monad
-- TODO
