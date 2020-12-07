module WebRow.HTTP.Response.BodyWriter where

import Prelude

import Data.Either (Either)
import Effect (Effect)
import Effect.Aff (Error) as Effect.Exceptions
import Effect.Aff (makeAff, nonCanceler) as Aff
import HTTPure.Body (class Body)
import HTTPure.Headers (header) as Headers
import Node.HTTP (responseAsStream) as HTTP
import Node.Stream (Writable) as Stream

type Done a = Either Effect.Exceptions.Error a → Effect Unit

type WriterContext = { done ∷ Done Unit, output ∷ Stream.Writable () }

type WriteHandler = (WriterContext → Effect Unit)

newtype BodyWriter = BodyWriter WriteHandler

instance bodyChunkedWriter ∷ Body BodyWriter where

  -- | defaultHeaders :: b -> Effect.Effect Headers.Headers
  defaultHeaders _ = pure $ Headers.header "Transfer-Encoding" "chunked"

  -- | write :: b -> HTTP.Response -> Aff.Aff Unit
  write (BodyWriter writer) response = Aff.makeAff \done -> do
    -- let stream = TypeEquals.to body
    let
      output = HTTP.responseAsStream response
    -- Stream.onEnd stream $ done $ Either.Right unit
    _ <- writer { done, output }
    pure Aff.nonCanceler

