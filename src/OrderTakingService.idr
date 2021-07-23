module OrderTakingService

import Language.JSON
import Data.String
import System

import Rango.BoundedContext.BoundedContext
import Rango.BoundedContext.Workflow
import Rango.DataTransfer.JSON.Interfaces

import Service.NodeJS.HTTP
import Service.NodeJS.SQLite
import Service.NodeJS.MD5
import Service.NodeJS.Date
import Service.NodeJS.Promise

import Rango.BoundedContext.BoundedContext
import BoundedContext.OrderTaking
import BoundedContext.OrderTaking.DTO
import BoundedContext.OrderTaking.ConvertDTO
import BoundedContext.OrderTaking.Command
import BoundedContext.OrderTaking.Error
import BoundedContext.OrderTaking.Event
import BoundedContext.OrderTaking.Workflow.PlaceOrder.Backend
import BoundedContext.OrderTaking.Workflow.PlaceOrder.DTO

import BoundedContext.OrderTaking.Workflow.PlaceOrder.Database.Order
import BoundedContext.OrderTaking.Workflow.PlaceOrder.Database.Product



handleCommand : Command.OrderTaking -> Promise (Either Error.OrderTaking Event.OrderTaking)
handleCommand = boundedContext orderTakingImpl

boundedContextHandler : Request -> Response -> IO ()
boundedContextHandler req rsp =
  resolve' (\_ => pure ()) (\err => putStrLn $ "Unhandled error has happened: \{err}") $ do
    content <- req.body
    putStrLn content
    rsp.setHeader "Content-Type" "application/json"
    rsp.setHeader "Access-Control-Allow-Origin" "*"
    let Just jsValue = JSON.parse content
        | Nothing => do
            putStrLn "Couldn't parse incoming JSON."
            rsp.setStatusCode 400
            rsp.end "{\"message\":\"Couldn't parse incoming JSON.\"}"
    Just cmd <- the (Promise (Maybe Command.OrderTaking))
              $ case Request.url req of
                  "/order-taking" => do
                    let Just commandDTO = the (Maybe CommandDTO) (fromJSON jsValue)
                        | Nothing => do
                            putStrLn "Couldn't parse JSValue."
                            rsp.setStatusCode 400
                            rsp.end "{\"message\":\"Couldn't parse Command DTO JSON.\"}"
                            pure Nothing
                    pure $ Just $ fromCommandDTO commandDTO
                  path  => do
                    rsp.setStatusCode 400
                    rsp.end $ "{\"message\":\"Unknown API endpoint: \{path} \"}"
                    pure Nothing
      | Nothing => pure ()
    result <- handleCommand cmd
    case result of
      Left err => do
        rsp.setStatusCode 400
        rsp.end $ format 0 $ JObject
          [ ("message", JString "There was a placed order error.")
          , ("order-event-error", toJSON $ toErrorDTO err)
          ]
      Right ev => do
        rsp.setStatusCode 200
        rsp.end $ format 0 $ toJSON $ toEventDTO ev

startService : IO ()
startService = do
  putStrLn "Staring Order taking service."
  http   <- HTTP.require
  server <- http.createServer boundedContextHandler
  server.listen 3000 "127.0.0.1"

initDB : IO ()
initDB = do
  BoundedContext.OrderTaking.Workflow.PlaceOrder.Database.Order.initDB 
  BoundedContext.OrderTaking.Workflow.PlaceOrder.Database.Product.initDB

main : IO ()
main = do
  args <- getArgs
  printLn args
  if "--init-db" `elem` args
    then OrderTakingService.initDB
    else startService
