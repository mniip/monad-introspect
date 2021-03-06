{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Control.Monad.Introspect.Class where

import Control.Applicative
import Control.Monad hiding (fail)
import Control.Monad.Error (ErrorT(..), mapErrorT)
import Control.Monad.Error.Class 
import Control.Monad.Except (ExceptT(..), mapExceptT)
import Control.Monad.Trans.Class
import Control.Monad.Trans.Compose
import Control.Monad.Trans.Cont (ContT(..), mapContT)
import Control.Monad.Trans.Identity (IdentityT(..), mapIdentityT)
import Control.Monad.Trans.List (ListT(..), mapListT)
import Control.Monad.Trans.Maybe (MaybeT(..), mapMaybeT)
import Control.Monad.Trans.Reader (ReaderT(..), mapReaderT)
import Control.Monad.Trans.RWS (RWST(..), mapRWST)
import qualified Control.Monad.Trans.State.Lazy as L
import qualified Control.Monad.Trans.State.Strict as S
import qualified Control.Monad.Trans.Writer.Lazy as L
import qualified Control.Monad.Trans.Writer.Strict as S
import Data.Coerce
import Prelude hiding (fail)
import Data.Type.Role.Representational

-- * Abstract interface

-- | A monad with introspection capability is able to query an environment @r@
-- that is parameterized by the monad itself, i.e. can contain effectful
-- functions.
--
-- The caveat is that sometimes the monad changes (e.g. we locally run a
-- transformer, or we globally run our transformer steck), so the monad in the
-- environment can "desync" from the ambient monad. This warrants a more general
-- class: 'MonadIntrospectTrans', of which 'MonadIntrospect' is a special case.
--
-- The machinery sometimes requires newtype wrapping/unwrapping the monad that
-- goes to the environment. We use/require 'Coercible' for that, as the
-- "functorial" operations are deemed expensive. The constraint
-- @'Representational' r@ ensures that the environment can be coerced provided
-- the monad can be coerced.
--
-- Otherwise the interface is identical to that of
-- 'Control.Monad.Reader.Class.MonadReader'.
class (Representational r, MonadIntrospectTrans IdentityT r m)
  => MonadIntrospect (r :: (* -> *) -> *) (m :: * -> *) where
  -- | Essentially 'Control.Monad.Reader.Class.ask'.
  introspect :: m (r m)
  -- | Essentially 'Control.Monad.Reader.Class.local'.
  substitute :: (r m -> r m) -> m a -> m a

-- | If the ambient monad is @m@ and the environment @r@ has additional effects
-- @t@, we can ask for that environment, or substitute it. Multiple (or zero)
-- effects can be combined into a single @t@ with 'ComposeT' (or 'IdentityT').
class (Monad m, MonadTrans t) => MonadIntrospectTrans
  (t :: (* -> *) -> * -> *)
  (r :: (* -> *) -> *)
  (m :: * -> *)
  | m -> t where
  -- | Essentially 'Control.Monad.Reader.Class.ask'.
  introspectTrans :: m (r (t m))
  -- | Essentially 'Control.Monad.Reader.Class.local'.
  substituteTrans :: (r (t m) -> r (t m)) -> m a -> m a

instance (Representational r, MonadIntrospectTrans IdentityT r m)
  => MonadIntrospect r m where
  introspect = liftTransEnv <$> introspectTrans
  substitute = substituteTrans . liftTransMapper

-- * Utility functions for coercing environments

liftTransEnv :: (Representational r, Coercible m n) => r m -> r n
liftTransEnv = coerce

liftTransMapper :: (Representational r, Coercible m n)
  => (r m -> r m) -> r n -> r n
liftTransMapper = coerce

-- Other effects proxy MonadIntrospect

instance
  ( Representational r
  , MonadTrans t
  , MonadIntrospectTrans (ComposeT t IdentityT) r m )
  => MonadIntrospectTrans t r (IdentityT m) where
  introspectTrans = lift $ liftTransEnv <$> introspectTrans
  substituteTrans = mapIdentityT . substituteTrans . liftTransMapper

instance
  ( Representational r
  , MonadTrans t
  , MonadIntrospectTrans (ComposeT t ListT) r m )
  => MonadIntrospectTrans t r (ListT m) where
  introspectTrans = lift $ liftTransEnv <$> introspectTrans
  substituteTrans = mapListT . substituteTrans . liftTransMapper

instance
  ( Representational r
  , MonadTrans t
  , MonadIntrospectTrans (ComposeT t (ContT e)) r m )
  => MonadIntrospectTrans t r (ContT e m) where
  introspectTrans = lift $ liftTransEnv <$> introspectTrans
  substituteTrans = mapContT . substituteTrans . liftTransMapper

instance
  ( Representational r
  , MonadTrans t
  , MonadIntrospectTrans (ComposeT t (ExceptT e)) r m )
  => MonadIntrospectTrans t r (ExceptT e m) where
  introspectTrans = lift $ liftTransEnv <$> introspectTrans
  substituteTrans = mapExceptT . substituteTrans . liftTransMapper

instance
  ( Representational r
  , Error e
  , MonadTrans t
  , MonadIntrospectTrans (ComposeT t (ErrorT e)) r m )
  => MonadIntrospectTrans t r (ErrorT e m) where
  introspectTrans = lift $ liftTransEnv <$> introspectTrans
  substituteTrans = mapErrorT . substituteTrans . liftTransMapper

instance
  ( Representational r
  , MonadTrans t
  , MonadIntrospectTrans (ComposeT t (ReaderT e)) r m )
  => MonadIntrospectTrans t r (ReaderT e m) where
  introspectTrans = lift $ liftTransEnv <$> introspectTrans
  substituteTrans = mapReaderT . substituteTrans . liftTransMapper

instance
  ( Representational r
  , MonadTrans t
  , MonadIntrospectTrans (ComposeT t MaybeT) r m )
  => MonadIntrospectTrans t r (MaybeT m) where
  introspectTrans = lift $ liftTransEnv <$> introspectTrans
  substituteTrans = mapMaybeT . substituteTrans . liftTransMapper

instance
  ( Representational r
  , MonadTrans t
  , MonadIntrospectTrans (ComposeT t (L.StateT s)) r m )
  => MonadIntrospectTrans t r (L.StateT s m) where
  introspectTrans = lift $ liftTransEnv <$> introspectTrans
  substituteTrans = L.mapStateT . substituteTrans . liftTransMapper

instance
  ( Representational r
  , MonadTrans t
  , MonadIntrospectTrans (ComposeT t (S.StateT s)) r m )
  => MonadIntrospectTrans t r (S.StateT s m) where
  introspectTrans = lift $ liftTransEnv <$> introspectTrans
  substituteTrans = S.mapStateT . substituteTrans . liftTransMapper

instance
  ( Representational r
  , Monoid w
  , MonadTrans t
  , MonadIntrospectTrans (ComposeT t (L.WriterT w)) r m )
  => MonadIntrospectTrans t r (L.WriterT w m) where
  introspectTrans = lift $ liftTransEnv <$> introspectTrans
  substituteTrans = L.mapWriterT . substituteTrans . liftTransMapper

instance
  ( Representational r
  , Monoid w
  , MonadTrans t
  , MonadIntrospectTrans (ComposeT t (S.WriterT w)) r m )
  => MonadIntrospectTrans t r (S.WriterT w m) where
  introspectTrans = lift $ liftTransEnv <$> introspectTrans
  substituteTrans = S.mapWriterT . substituteTrans . liftTransMapper

instance
  ( Representational r
  , Monoid w
  , MonadTrans t
  , MonadIntrospectTrans (ComposeT t (RWST e w s)) r m )
  => MonadIntrospectTrans t r (RWST e w s m) where
  introspectTrans = lift $ liftTransEnv <$> introspectTrans
  substituteTrans = mapRWST . substituteTrans . liftTransMapper
