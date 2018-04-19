{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module Log.Configuration.Extension
       ( Extension (..)
       , ElemExtension
       , ExtensionType

       , HasRotation
       , HasTerminal
       ) where

import Universum

import Data.Type.Bool (If)

-- | Kind for extension points which are used in logger configuration.
data Extension = Rotation | Terminal

-- | Type family for checking whether given 'Extension' is in list of extensions.
type family ElemExtension (ext :: Extension) (exts :: [Extension]) :: Bool where
    ElemExtension ext '[]           = 'False
    ElemExtension ext (ext ': exts) = 'True
    ElemExtension ext (e   ': exts) = ElemExtension ext exts

-- | If given extension @ext@ is in list of extensions @exts@ then return
-- provided type @param@. Otherwise return type @noExt@.
type family ExtensionType
    (ext   :: Extension)
    (exts  :: [Extension])
    (param :: Type)
    (noExt :: Type) :: Type
  where
    ExtensionType ext exts param noExt = If (ElemExtension ext exts) param noExt

-- | Ensures that given list of extensions has 'Rotation' extension.
type HasRotation exts = ElemExtension 'Rotation exts ~ 'True

-- | Ensures that given list of extensions has 'Terminal' extension.
type HasTerminal exts = ElemExtension 'Terminal exts ~ 'True
