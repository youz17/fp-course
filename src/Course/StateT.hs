{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Course.StateT where

import Control.Monad (when)
import Course.Applicative
import Course.Core
import Course.ExactlyOne
import Course.Functor
import Course.List
import Course.Monad
import Course.Optional
import Data.Bifunctor qualified (first)
import Data.Data (funResultTy)
import Data.Set qualified as S
import Data.Traversable (foldMapDefault)
import Prelude qualified as P

-- $setup
-- >>> import Test.QuickCheck
-- >>> import qualified Prelude as P(fmap)
-- >>> instance Arbitrary a => Arbitrary (List a) where arbitrary = P.fmap listh arbitrary

-- | A `StateT` is a function from a state value `s` to a functor f of (a produced value `a`, and a resulting state `s`).
newtype StateT s f a = StateT
  { runStateT ::
      s ->
      f (a, s)
  }

-- | Implement the `Functor` instance for @StateT s f@ given a @Functor f@.
--
-- >>> runStateT ((+1) <$> (pure 2) :: StateT Int List Int) 0
-- [(3,0)]
instance Functor f => Functor (StateT s f) where
  (<$>) ::
    (a -> b) ->
    StateT s f a ->
    StateT s f b
  (<$>) f sa = StateT fun
    where
      fun s = first f <$> runStateT sa s

-- | Implement the `Applicative` instance for @StateT s f@ given a @Monad f@.
--
-- >>> runStateT (pure 2) 0
-- (2,0)
--
-- >>> runStateT ((pure 2) :: StateT Int List Int) 0
-- [(2,0)]
--
-- >>> runStateT (pure (+2) <*> ((pure 2) :: StateT Int List Int)) 0
-- [(4,0)]
--
-- >>> import qualified Prelude as P
-- >>> runStateT (StateT (\s -> Full ((+2), s P.++ [1])) <*> (StateT (\s -> Full (2, s P.++ [2])))) [0]
-- Full (4,[0,1,2])
--
-- >>> runStateT (StateT (\s -> ((+2), s P.++ [1]) :. ((+3), s P.++ [1]) :. Nil) <*> (StateT (\s -> (2, s P.++ [2]) :. Nil))) [0]
-- [(4,[0,1,2]),(5,[0,1,2])]
instance Monad f => Applicative (StateT s f) where
  pure ::
    a ->
    StateT s f a
  pure a = StateT (\s -> pure (a, s))
  (<*>) ::
    StateT s f (a -> b) ->
    StateT s f a ->
    StateT s f b
  (<*>) sf sa = StateT fun
    where
      fun s = runStateT sf s >>= fun'
      fun' (f, s) = first f <$> runStateT sa s

-- | Implement the `Monad` instance for @StateT s f@ given a @Monad f@.
-- Make sure the state value is passed through in `bind`.
--
-- >>> runStateT ((const $ putT 2) =<< putT 1) 0
-- ((),2)
--
-- >>> let modify f = StateT (\s -> pure ((), f s)) in runStateT (modify (+1) >>= \() -> modify (*2)) 7
-- ((),16)
instance Monad f => Monad (StateT s f) where
  (=<<) ::
    (a -> StateT s f b) ->
    StateT s f a ->
    StateT s f b
  (=<<) f sa = StateT fun
    where
      fun s = runStateT sa s >>= runStateTfa
      runStateTfa (a, s) = runStateT (f a) s

-- | A `State'` is `StateT` specialised to the `ExactlyOne` functor.
type State' s a =
  StateT s ExactlyOne a

-- | Provide a constructor for `State'` values
--
-- >>> runStateT (state' $ runState $ put 1) 0
-- ExactlyOne  ((),1)
state' ::
  (s -> (a, s)) ->
  State' s a
state' f = StateT (ExactlyOne . f)

-- | Provide an unwrapper for `State'` values.
--
-- >>> runState' (state' $ runState $ put 1) 0
-- ((),1)
runState' ::
  State' s a ->
  s ->
  (a, s)
runState' sa s = runExactlyOne $ runStateT sa s

-- | Run the `StateT` seeded with `s` and retrieve the resulting state.
execT ::
  Functor f =>
  StateT s f a ->
  s ->
  f s
execT sa s = snd <$> runStateT sa s

-- | Run the `State` seeded with `s` and retrieve the resulting state.
exec' ::
  State' s a ->
  s ->
  s
exec' sa s = runExactlyOne $ execT sa s

-- | Run the `StateT` seeded with `s` and retrieve the resulting value.
evalT ::
  Functor f =>
  StateT s f a ->
  s ->
  f a
evalT sa s = fst <$> runStateT sa s

-- | Run the `State` seeded with `s` and retrieve the resulting value.
eval' ::
  State' s a ->
  s ->
  a
eval' sa s = runExactlyOne $ evalT sa s

-- | A `StateT` where the state also distributes into the produced value.
--
-- >>> (runStateT (getT :: StateT Int List Int) 3)
-- [(3,3)]
getT ::
  Applicative f =>
  StateT s f s
getT = StateT (pure . (\s -> (s, s)))

-- | A `StateT` where the resulting state is seeded with the given value.
--
-- >>> runStateT (putT 2) 0
-- ((),2)
--
-- >>> runStateT (putT 2 :: StateT Int List ()) 0
-- [((),2)]
putT ::
  Applicative f =>
  s ->
  StateT s f ()
putT s = StateT (\_ -> pure ((), s))

-- | Remove all duplicate elements in a `List`.
--
-- /Tip:/ Use `filtering` and `State'` with a @Data.Set#Set@.
--
-- prop> \xs -> distinct' xs == distinct' (flatMap (\x -> x :. x :. Nil) xs)
distinct' ::
  (Ord a, Num a) =>
  List a ->
  List a
distinct' l = eval' (filtering genState l) S.empty
  where
    genState e = state' (fun e)
    fun e s = if S.member e s then (False, s) else (True, S.insert e s)

-- | Remove all duplicate elements in a `List`.
-- However, if you see a value greater than `100` in the list,
-- abort the computation by producing `Empty`.
--
-- /Tip:/ Use `filtering` and `StateT` over `Optional` with a @Data.Set#Set@.
--
-- >>> distinctF $ listh [1,2,3,2,1]
-- Full [1,2,3]
--
-- >>> distinctF $ listh [1,2,3,2,1,101]
-- Empty
distinctF ::
  (Ord a, Num a) =>
  List a ->
  Optional (List a)
distinctF l = evalT (filtering genState l) S.empty
  where
    genState e = StateT (fun e)
    fun e s
      | e > 100 = Empty
      | S.member e s = Full (False, s)
      | otherwise = Full (True, S.insert e s)

-- | An `OptionalT` is a functor of an `Optional` value.
data OptionalT f a = OptionalT
  { runOptionalT ::
      f (Optional a)
  }

-- | Implement the `Functor` instance for `OptionalT f` given a Functor f.
--
-- >>> runOptionalT $ (+1) <$> OptionalT (Full 1 :. Empty :. Nil)
-- [Full 2,Empty]
instance Functor f => Functor (OptionalT f) where
  (<$>) :: Functor f => (a -> b) -> OptionalT f a -> OptionalT f b
  (<$>) f ot = OptionalT ((f <$>) <$> runOptionalT ot)

-- | Implement the `Applicative` instance for `OptionalT f` given a Monad f.
--
-- /Tip:/ Use `onFull` to help implement (<*>).
--
-- >>> runOptionalT $ OptionalT Nil <*> OptionalT (Full 1 :. Full 2 :. Nil)
-- []
--
-- >>> runOptionalT $ OptionalT (Full (+1) :. Full (+2) :. Nil) <*> OptionalT Nil
-- []
--
-- >>> runOptionalT $ OptionalT (Empty :. Nil) <*> OptionalT (Empty :. Nil)
-- [Empty]
--
-- >>> runOptionalT $ OptionalT (Full (+1) :. Empty :. Nil) <*> OptionalT (Empty :. Nil)
-- [Empty,Empty]
--
-- >>> runOptionalT $ OptionalT (Empty :. Nil) <*> OptionalT (Full 1 :. Full 2 :. Nil)
-- [Empty]
--
-- >>> runOptionalT $ OptionalT (Full (+1) :. Empty :. Nil) <*> OptionalT (Full 1 :. Full 2 :. Nil)
-- [Full 2,Full 3,Empty]
--
-- >>> runOptionalT $ OptionalT (Full (+1) :. Full (+2) :. Nil) <*> OptionalT (Full 1 :. Empty :. Nil)
-- [Full 2,Empty,Full 3,Empty]
instance Monad f => Applicative (OptionalT f) where
  pure a = OptionalT (pure (Full a))
  (<*>) :: Monad f => OptionalT f (a -> b) -> OptionalT f a -> OptionalT f b
  (<*>) fopf fopa = OptionalT result
    where
      isEmpty' Empty = True
      isEmpty' _ = False
      result = do
        opf <- runOptionalT fopf
        if isEmpty' opf
          then return Empty
          else do
            opa <- runOptionalT fopa
            return $ opf <*> opa

-- | Implement the `Monad` instance for `OptionalT f` given a Monad f.
--
-- >>> runOptionalT $ (\a -> OptionalT (Full (a+1) :. Full (a+2) :. Nil)) =<< OptionalT (Full 1 :. Empty :. Nil)
-- [Full 2,Full 3,Empty]
instance Monad f => Monad (OptionalT f) where
  (=<<) :: Monad f => (a -> OptionalT f b) -> OptionalT f a -> OptionalT f b
  (=<<) f fopa = OptionalT $ runOptionalT fopa >>= fun
    where
      fun (Full a) = runOptionalT $ f a
      fun Empty = pure Empty

-- | A `Logger` is a pair of a list of log values (`[l]`) and an arbitrary value (`a`).
data Logger l a
  = Logger (List l) a
  deriving (Eq, Show)

-- | Implement the `Functor` instance for `Logger
--
-- >>> (+3) <$> Logger (listh [1,2]) 3
-- Logger [1,2] 6
instance Functor (Logger l) where
  (<$>) f (Logger l a) = Logger l (f a)

-- | Implement the `Applicative` instance for `Logger`.
--
-- >>> pure "table" :: Logger Int P.String
-- Logger [] "table"
--
-- >>> Logger (listh [1,2]) (+7) <*> Logger (listh [3,4]) 3
-- Logger [1,2,3,4] 10
instance Applicative (Logger l) where
  pure = Logger Nil
  (<*>) (Logger la f) (Logger lb a) = Logger (la ++ lb) (f a)

-- | Implement the `Monad` instance for `Logger`.
-- The `bind` implementation must append log values to maintain associativity.
--
-- >>> (\a -> Logger (listh [4,5]) (a+3)) =<< Logger (listh [1,2]) 3
-- Logger [1,2,4,5] 6
instance Monad (Logger l) where
  (=<<) f (Logger l a) = let (Logger l' b) = f a in Logger (l ++ l') b

-- | A utility function for producing a `Logger` with one log value.
--
-- >>> log1 1 2
-- Logger [1] 2
log1 ::
  l ->
  a ->
  Logger l a
log1 l = Logger (l :. Nil)

-- | Remove all duplicate integers from a list. Produce a log as you go.
-- If there is an element above 100, then abort the entire computation and produce no result.
-- However, always keep a log. If you abort the computation, produce a log with the value,
-- "aborting > 100: " followed by the value that caused it.
-- If you see an even number, produce a log message, "even number: " followed by the even number.
-- Other numbers produce no log message.
--
-- /Tip:/ Use `filtering` and `StateT` over (`OptionalT` over `Logger` with a @Data.Set#Set@).
--
-- >>> distinctG $ listh [1,2,3,2,6]
-- Logger ["even number: 2","even number: 2","even number: 6"] (Full [1,2,3,6])
--
-- >>> distinctG $ listh [1,2,3,2,6,106]
-- Logger ["even number: 2","even number: 2","even number: 6","aborting > 100: 106"] Empty
distinctG ::
  (Integral a, Show a) =>
  List a ->
  Logger Chars (Optional (List a))
distinctG l = (fst <$>) <$> result
  where
    result = runOptionalT $ runStateT (filtering genState l) S.empty
    genState e = StateT (OptionalT . fun e)
    genLog :: Show x => P.String -> x -> List Chars
    genLog s n = listh (s P.++ show n) :. Nil
    fun e s
      | e > 100 = Logger (genLog "aborting > 100: " e) Empty
      | S.member e s = Logger (genLog "even number: " e) (Full (False, s))
      | otherwise = Logger (if even e then genLog "even number: " e else Nil) (Full (True, S.insert e s))

onFull ::
  Applicative f =>
  (t -> f (Optional a)) ->
  Optional t ->
  f (Optional a)
onFull g o =
  case o of
    Empty ->
      pure Empty
    Full a ->
      g a
