{-# LANGUAGE InstanceSigs #-}
module Sandbox where

data Maybe' a = Nothing' | Just' a deriving(Read, Show)

data Either' e a = Left' e | Right' a deriving(Read, Show)

instance Functor Maybe' where
  fmap :: ( a -> b) -> Maybe' a -> Maybe' b
  fmap _ Nothing' = Nothing'
  fmap f (Just' x) = Just' (f x)

instance Applicative Maybe' where
  pure :: a -> Maybe' a 
  pure x = Just' x

  (<*>) :: Maybe' ( a -> b) -> Maybe' a -> Maybe' b  -- app
  Just' f <*> mA = f <$> mA -- handles case of mA == Nothing'
  _ <*> _ = Nothing'
  {- (<*>) Nothing' _ = Nothing'
  (<*>) _ Nothing' = Nothing'
  (<*>) (Just' f) (Just' x) = Just' (f x) -}

instance Monad Maybe' where
  (>>=) :: Maybe' a -> (a -> Maybe' b) -> Maybe' b 
  (>>=) Nothing' _ = Nothing'
  (>>=) (Just' x) f = f x

instance Functor (Either' e) where
  fmap :: ( a -> b) -> (Either' e) a -> (Either' e) b
  fmap _ (Left' e) = Left' e 
  fmap f (Right' b) = Right' $ f b 

instance Applicative (Either' e) where
  pure :: a -> (Either' e) a
  pure x = Right' x

  (<*>) :: (Either' e) ( a -> b) -> (Either' e) a -> (Either' e) b
  Right' f <*> eEx = f <$> eEx
  Left' e <*> _ = Left' e
  {- (<*>) (Left' e) _ = Left' e 
  (<*>) _ (Left' e) = Left' e 
  (<*>) (Right' f) (Right' x) = Right' $ f x -}

instance Monad (Either' e) where
  (>>=) :: (Either' e) a -> ( a -> (Either' e) b ) -> (Either' e) b 
  (>>=) (Left' e) _ = Left' e 
  (>>=) (Right' x) f = f x