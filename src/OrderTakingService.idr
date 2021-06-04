module OrderTakingService

import OrderTaking.Domain.PlaceOrder
import OrderTaking.BoundedContext.PlaceOrder
import Service.NodeJS.HTTP
import Service.NodeJS.SQLite
import Service.NodeJS.MD5
import Service.NodeJS.Date
import OrderTaking.Database.Order
import OrderTaking.Database.Product
import OrderTaking.Domain.Backend
import Language.JSON
import Rango.BoundedContext.Workflow
import Data.String
import System


printError : String -> Error -> IO ()
printError _ err = case !(occured err) of
  Nothing => pure ()
  Just e  => putStrLn !(toString e)

onRow : Error -> Row -> IO ()
onRow err row = do
  Nothing <- occured err
    | Just e => putStrLn !(toString e)
  Just r <- nonEmpty row
    | _ => putStrLn "Empty row."
  putStrLn !(toString r)
  putStrLn (show !(json r))

onCompleted : Error -> IO ()
onCompleted err = do
  Nothing <- occured err
    | Just e => putStrLn !(toString e)
  putStrLn "Found entries."

orderTaking : (RunBackend (List PlacedOrderEvent)) -> Request -> Response -> IO ()
orderTaking runBackend req rsp = do
  let addressForm = MkAddressForm
        { addressLine1 = "Bright Street 55."
        , addressLine2 = Nothing
        , addressLine3 = Nothing
        , addressLine4 = Nothing
        , city         = "Los Angeles"
        , zipCode      = "ZP-55-555"
        }
  let cusomterInfoForm = MkCustomerInfoForm 
        { firstName = "John"
        , lastName = "Doe"
        , emailAddress = "john.doe@emial.com"
        }
  let orderLines = [ MkOrderLineForm
        { productCode = "G125"
        , quantity    = "3"
        } ]
  let orderForm = MkOrderForm
        { customerInfo = cusomterInfoForm
        , shippingAddress = addressForm
        , billingAddress  = addressForm
        , orderLines      = orderLines
        }
  orderEvents <- runBackend $ orderTakingWorkflow orderForm
  Response.statusCode rsp 200
  Response.setHeader  rsp "Content-Type" "text/plain"
  case orderEvents of
    Left err     => Response.end rsp $ "There was an error: " ++ show err
    Right events => Response.end rsp $ unlines $ "Your order has taken!" :: map show events
  where
    orderTakingWorkflow : OrderForm -> Backend (List PlacedOrderEvent)
    orderTakingWorkflow = interpret backend . morph withPOMMapping PlaceOrder.workflow

initDB : IO ()
initDB = do
  Database.Order.initDB
  Database.Product.initDB

startService : IO ()
startService = do
  putStrLn "Staring Order taking service."
  run <- mkRunBackend
  http   <- HTTP.require
  server <- HTTP.createServer http (orderTaking run)
  Server.listen server 3000 "127.0.0.1"

main : IO ()
main = do
  args <- getArgs
  printLn args
  if "--init-db" `elem` args
    then OrderTakingService.initDB
    else startService
