-- |
-- This module provides means to generate and manipulate
-- Swagger specification for servant APIs.
--
-- Swagger™ is a project used to describe and document RESTful APIs.
--
-- The Swagger specification defines a set of files required to describe such an API.
-- These files can then be used by the Swagger-UI project to display the API
-- and Swagger-Codegen to generate clients in various languages.
-- Additional utilities can also take advantage of the resulting files, such as testing tools.
--
-- For more information see <http://swagger.io/ Swagger™ documentation>.
module Servant.Swagger (
  HasSwagger(..),

  subOperations,

  ToResponseHeader(..),
  AllAccept,
  AllToResponseHeader,
) where

import Servant.Swagger.Internal

