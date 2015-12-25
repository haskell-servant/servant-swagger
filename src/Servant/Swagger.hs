module Servant.Swagger (
  HasSwagger(..),

  addTag,
  subOperations,

  ToResponseHeader(..),
  AllAccept,
  AllToResponseHeader,
) where

import Servant.Swagger.Internal

