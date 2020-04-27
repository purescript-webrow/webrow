module ShopUtils.Payment where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Variant (SProxy(..), inj)
import Routing.Duplex as D
import Routing.Duplex.Generic as DG
import Run (FProxy, Run)
import Run as Run
import Run.Reader (READER, ask)
import ShopUtils.Logging.Effect (LOGGER)
import ShopUtils.Logging.Effect as LogEff
import ShopUtils.Response (RESPONSE, response)

data Route
  = Payment
  -- | PaymentOngoing
derive instance genericRoute ∷ Generic Route _

route ∷ D.RouteDuplex' Route
route = D.path "register" $ DG.sum
  { "Payment": DG.noArgs
  -- , "PaymentOngoing": D.path "ongoing" DG.noArgs
  }

data DotPayRoute
  = DotPayPayment -- TODO: change to safer/custom types 
      { id ∷ Int
      , amount ∷ String
      , currency ∷ String
      , description ∷ String
      -- , url ∷ String
      }
derive instance genericDotPayRoute ∷ Generic DotPayRoute _

dotpayRoute ∷ D.RouteDuplex' DotPayRoute
dotpayRoute = D.path "https://ssl.dotpay.pl/t2" $ DG.sum
  { "DotPayPayment": D.params
      { id: D.int
      , amount: D.string
      , currency: D.string
      , description: D.string
      -- , url: D.string
      }
  }

onPaymentRoute
  ∷ ∀ res eff a ctx
  . Route
  → Run
      ( logger ∷ LOGGER
      , payment ∷ PAYMENT
      , reader ∷ READER { shopId ∷ Int | ctx }
      , response ∷ RESPONSE ( redirect ∷ String | res )
      | eff
      )
      a
onPaymentRoute = case _ of
  -- PaymentOngoing → 
  Payment → do
    r ← orderSummary
    ctx ← ask
    let
      dotpayLink = D.print dotpayRoute $ DotPayPayment
        { id: ctx.shopId
        , amount: show r.amount <> ".00"
        , currency: r.currency
        , description: r.orderId
        }
    LogEff.info $ "New payment: " <> r.orderId <> " " <> dotpayLink
    response $ inj (SProxy ∷ SProxy "redirect") dotpayLink

data PaymentF a
  = OrderSummary (OrderSummary → a)
derive instance functorPaymentF ∷ Functor PaymentF

type PAYMENT = FProxy PaymentF

_payment = SProxy ∷ SProxy "payment"

type OrderSummary =
  { orderId ∷ String
  , amount ∷ Int -- TODO: change to decimal
  , currency ∷ String -- TODO: change to custom type
  }

orderSummary ∷ ∀ eff. Run ( payment ∷ PAYMENT | eff ) OrderSummary
orderSummary = Run.lift _payment (OrderSummary identity)
