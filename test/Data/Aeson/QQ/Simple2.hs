-- | Like "Data.Aeson.QQ" but without interpolation.
module Data.Aeson.QQ.Simple2 (aesonQQ) where

import           Data.Aeson (eitherDecode, Value)
import           Data.ByteString.Lazy.UTF8  as UTF8
import           Language.Haskell.TH
import           Language.Haskell.TH.Quote
import           Language.Haskell.TH.Syntax (Lift (..))
import           Prelude                    ()
import           Prelude.Compat

aesonQQ :: QuasiQuoter
aesonQQ = QuasiQuoter
    { quoteExp  = aesonExp
    , quotePat  = const $ error "No quotePat defined for jsonQQ"
    , quoteType = const $ error "No quoteType defined for jsonQQ"
    , quoteDec  = const $ error "No quoteDec defined for jsonQQ"
    }

aesonExp :: String -> ExpQ
aesonExp txt =
  case eitherDecode $ UTF8.fromString txt of
    Left err  -> error $ "Error in aesonExp: " ++ show err
    Right val -> lift (val :: Value)
