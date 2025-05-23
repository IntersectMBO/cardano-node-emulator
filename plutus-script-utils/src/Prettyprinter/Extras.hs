{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Copied from plutus-ledger-api because not exported
module Prettyprinter.Extras
  ( PrettyShow (..),
    Pretty (..),
    PrettyFoldable (..),
    Tagged (Tagged),
  )
where

import Data.Foldable (Foldable (toList))
import Data.Proxy (Proxy (Proxy))
import Data.String (IsString (fromString))
import Data.Tagged (Tagged (Tagged))
import GHC.TypeLits (KnownSymbol, symbolVal)
import Prettyprinter (Doc, Pretty, pretty, viaShow, (<+>))

-- | Newtype wrapper for deriving 'Pretty' via a 'Show' instance
newtype PrettyShow a = PrettyShow {unPrettyShow :: a}

instance (Show a) => Pretty (PrettyShow a) where
  pretty = viaShow . unPrettyShow

-- | Newtype wrapper for deriving 'Pretty' for a 'Foldable' container by
--  calling 'toList'.
newtype PrettyFoldable f a = PrettyFoldable {unPrettyFoldable :: f a}

instance (Foldable f, Pretty a) => Pretty (PrettyFoldable f a) where
  pretty = pretty . toList . unPrettyFoldable

instance (KnownSymbol a, Pretty b) => Pretty (Tagged a b) where
  pretty = prettyTagged

prettyTagged :: forall a b ann. (KnownSymbol a, Pretty b) => Tagged a b -> Doc ann
prettyTagged (Tagged b) = fromString (symbolVal (Proxy @a)) <+> pretty b
