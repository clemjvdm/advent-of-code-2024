-- Wrote my little monad which is equivalent to the Maybe monad

data Foo a = Bar a | Baz
instance Functor Foo where  -- Foo functor
  fmap f (Bar a) = Bar (f a)
  fmap f Baz = Baz

instance Monad Foo where
  Baz >>= f = Baz 
  (Bar a) >>= f = f a

instance Applicative Foo where
  Baz <*> _ = Baz
  _ <*> Baz = Baz
  Bar f <*> Bar a = Bar (f a)
  liftA2 f (Bar a) (Bar b) = Bar (f a b)
  liftA2 _ Baz _ = Baz
  pure = Bar
