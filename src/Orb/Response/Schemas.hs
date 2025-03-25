{-# LANGUAGE OverloadedStrings #-}

module Orb.Response.Schemas
  ( BadRequestMessage (..)
  , badRequestMessageSchema
  , ConflictMessage (..)
  , conflictMessageSchema
  , InternalServerError (..)
  , internalServerErrorSchema
  , NoContent (..)
  , NotFoundMessage (..)
  , notFoundMessageSchema
  , ServiceUnavailableError (..)
  , serviceUnavailableErrorSchema
  , SuccessMessage (..)
  , successMessageSchema
  , UnauthorizedMessage (..)
  , unauthorizedMessageSchema
  , UnprocessableContentMessage (..)
  , unprocessableContentSchema
  )
where

import Data.Text qualified as T
import Fleece.Core ((#+))
import Fleece.Core qualified as FC

newtype BadRequestMessage = BadRequestMessage
  {badRequestMessage :: T.Text}

badRequestMessageSchema :: FC.Fleece schema => schema BadRequestMessage
badRequestMessageSchema =
  FC.object $
    FC.constructor BadRequestMessage
      #+ FC.required "bad_request" badRequestMessage FC.text

newtype ConflictMessage = ConflictMessage
  {conflictMessage :: T.Text}

conflictMessageSchema :: FC.Fleece schema => schema ConflictMessage
conflictMessageSchema =
  FC.object $
    FC.constructor ConflictMessage
      #+ FC.required "conflict" conflictMessage FC.text

data InternalServerError = InternalServerError

internalServerErrorSchema :: FC.Fleece schema => schema InternalServerError
internalServerErrorSchema =
  FC.object $
    FC.constructor (const InternalServerError)
      #+ FC.required "internal_server_error" (const "Please contact support") FC.text

data NoContent = NoContent

newtype NotFoundMessage = NotFoundMessage
  {notFoundMessage :: T.Text}

notFoundMessageSchema :: FC.Fleece schema => schema NotFoundMessage
notFoundMessageSchema =
  FC.object $
    FC.constructor NotFoundMessage
      #+ FC.required "not_found" notFoundMessage FC.text

newtype ServiceUnavailableError = ServiceUnavailableError
  {serviceUnavailableText :: T.Text}

serviceUnavailableErrorSchema :: FC.Fleece schema => schema ServiceUnavailableError
serviceUnavailableErrorSchema =
  FC.object $
    FC.constructor ServiceUnavailableError
      #+ FC.required "service_error" serviceUnavailableText FC.text

newtype SuccessMessage = SuccessMessage
  {successMessage :: T.Text}

successMessageSchema :: FC.Fleece schema => schema SuccessMessage
successMessageSchema =
  FC.object $
    FC.constructor SuccessMessage
      #+ FC.required "success" successMessage FC.text

newtype UnauthorizedMessage = UnauthorizedMessage
  {unauthorizedMessage :: T.Text}

unauthorizedMessageSchema :: FC.Fleece schema => schema UnauthorizedMessage
unauthorizedMessageSchema =
  FC.object $
    FC.constructor UnauthorizedMessage
      #+ FC.required "unauthorized" unauthorizedMessage FC.text

newtype UnprocessableContentMessage = UnprocessableContentMessage
  {unprocessableContentText :: T.Text}

unprocessableContentSchema :: FC.Fleece schema => schema UnprocessableContentMessage
unprocessableContentSchema =
  FC.object $
    FC.constructor UnprocessableContentMessage
      #+ FC.required "unprocessable_content" unprocessableContentText FC.text
