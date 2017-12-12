module Main where

import Prelude

import Data.Identity (Identity)
import Data.List (List)

import Data.Symbol

import Data.Record (set)
import Data.Record.Builder

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)




data Compose (ms :: # Type) (m :: Type -> Type) a = Compose { | ms } (m a)

type Compose' ms a = Compose ms Identity a



instance composeFunctor :: (Functor m) => Functor (Compose ms m) where
  map f (Compose ms ma) = Compose ms $ map f ma


instance composeApply :: (Apply m) => Apply (Compose ms m) where
  apply (Compose ms ab) (Compose ms' a) = Compose (build (merge ms') ms) (ab a)


instance composeApplicative :: (Applicative m) => Applicative (Compose ms m) where
  pure a = Compose {} (pure a)


instance composeBind :: (Bind m) => Bind (Compose ms m) where
  bind f (Compose ma) = Compose $ bind f ma


instance composeMonad :: (Monad m) => Monad (Compose ms m)




class Composable m a (n :: # Type) | m a -> n where

  run  :: forall r m' b. Compose ( n r ) m' b -> m a

  with :: forall r m' b. Compose ( r ) m' b -> m a -> Compose ( n r ) m' b 



instance listCompose :: Composable List a (list :: List a) where

  run (Compose { list: l } _) = l

  with (Compose ms ma) l = Compose ms { list = l } ma




lift :: forall a m n b m' n' r. Composable m a n => Composable m' b n' => Compose ( n n' r ) m' b -> Compose ( n n' r ) m a
lift c@(Compose ms ma) = Compose (set (SProxy :: SProxy n') ma ms) (run c)






main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  log "Hello sailor!"
