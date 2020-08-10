{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Control.Monad.Trans.Introspect where

import Control.Applicative
import Control.Monad hiding (fail)
import Control.Monad.Cont.Class 
import Control.Monad.Error.Class 
import Control.Monad.Fail
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Reader.Class
import Control.Monad.Signatures
import Control.Monad.State.Class
import Control.Monad.Trans.Class
import Control.Monad.Writer.Class
import Control.Monad.Zip
import Data.Coerce
import Prelude hiding (fail)
import Control.Monad.Introspect.Class
import Data.Type.Role.Representational

-- * Concrete interface

-- | @'IntrospectT' t r m a@ extends the monad @m@ with access to an environment
-- @r@ parameterized by @m@ with additional effects @t@ on top.
newtype IntrospectT
  (t :: (* -> *) -> * -> *)
  (r :: (* -> *) -> *)
  (m :: * -> *)
  (a :: *)
  = IntrospectT { runIntrospectT :: r (t (IntrospectT t r m)) -> m a }

-- | Run an 'IntrospectT'. If introspection is the outermost effect then you
-- will likely have @t ~ 'Control.Monad.Trans.Identity.IdentityT'@ and thus you
-- can pick @n ~ 'IntrospectT' t r m@.
runIntrospect :: (Representational r, Coercible (t (IntrospectT t r m)) n)
  => r n -> IntrospectT t r m a -> m a
runIntrospect e (IntrospectT h) = h $ liftTransEnv e

instance Functor m => Functor (IntrospectT t r m) where
  fmap f (IntrospectT h) = IntrospectT $ fmap f . h

instance Applicative m => Applicative (IntrospectT t r m) where
  pure x = IntrospectT $ const $ pure x
  IntrospectT f <*> IntrospectT x = IntrospectT $ liftA2 (<*>) f x

instance Alternative m => Alternative (IntrospectT t r m) where
  empty = IntrospectT $ const empty
  IntrospectT f <|> IntrospectT g = IntrospectT $ liftA2 (<|>) f g

instance Monad m => Monad (IntrospectT t r m) where
  IntrospectT k >>= f = IntrospectT $ \e -> k e >>= \x -> runIntrospectT (f x) e

instance MonadPlus m => MonadPlus (IntrospectT t r m) where
  mzero = IntrospectT $ const mzero
  mplus (IntrospectT f) (IntrospectT g) = IntrospectT $ liftA2 mplus f g

instance MonadTrans (IntrospectT t r) where
  lift k = IntrospectT $ const k

instance (Monad m, MonadTrans t)
  => MonadIntrospectTrans t r (IntrospectT t r m) where
  introspectTrans = IntrospectT return
  substituteTrans f (IntrospectT h) = IntrospectT $ h . f

-- * Utility functions for proxying other effects

mapIntrospectT :: (m a -> m b) -> IntrospectT t r m a -> IntrospectT t r m b
mapIntrospectT f (IntrospectT h) = IntrospectT $ f . h

liftCallCC :: CallCC m a b -> CallCC (IntrospectT t r m) a b
liftCallCC cCC f = IntrospectT $ \r ->
  cCC $ \c -> runIntrospectT (f (IntrospectT . const . c)) r

liftCatch :: Catch e m a -> Catch e (IntrospectT t r m) a
liftCatch f m h = IntrospectT $ \r ->
  f (runIntrospectT m r) $ \e -> runIntrospectT (h e) r

-- IntrospectT proxies other effects

instance MonadError e m => MonadError e (IntrospectT t r m) where
  throwError = lift . throwError
  catchError = liftCatch catchError

instance MonadReader e m => MonadReader e (IntrospectT t r m) where
  ask = lift ask
  local = mapIntrospectT . local
  reader = lift . reader

instance MonadState s m => MonadState s (IntrospectT t r m) where
   get = lift get
   put = lift . put
   state = lift . state

instance MonadWriter w m => MonadWriter w (IntrospectT t r m) where
   writer = lift . writer
   tell = lift . tell
   listen = mapIntrospectT listen
   pass = mapIntrospectT pass

instance MonadCont m => MonadCont (IntrospectT t r m) where
  callCC = liftCallCC callCC

instance MonadFix m => MonadFix (IntrospectT t r m) where
  mfix f = IntrospectT $ \r -> mfix $ \a -> runIntrospectT (f a) r

instance MonadFail m => MonadFail (IntrospectT t r m) where
  fail = lift . fail

instance MonadZip m => MonadZip (IntrospectT t r m) where
  mzip (IntrospectT f) (IntrospectT g) = IntrospectT $ \r -> mzip (f r) (g r)
  mzipWith h (IntrospectT f) (IntrospectT g) = IntrospectT $ \r ->
    mzipWith h (f r) (g r)

instance MonadIO m => MonadIO (IntrospectT t r m) where
  liftIO = lift . liftIO
