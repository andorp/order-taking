module OrderTakingService

import System
import Language.JSON

import Service.NodeJS.HTTP
import Service.NodeJS.Promise

import Rango.DataTransfer.JSON.Interfaces
import Rango.BoundedContext.BoundedContext

import BoundedContext.OrderTaking
import BoundedContext.OrderTaking.DTO
import BoundedContext.OrderTaking.ConvertDTO
import BoundedContext.OrderTaking.Workflow.PlaceOrder.Database.Order
import BoundedContext.OrderTaking.Workflow.PlaceOrder.Database.Product

-- Although this is the main entry for the Order Taking Service, this is module is more like scaffolding
-- for the implementation of the Bounded Context idea.
-- Scaffolding because this module either initializes the SQLlite databases for the service
-- or starts the service instanciating an HTTP server with one attached listener, but
-- no depedently typed programming was applied here. The minimum requirements are implemented
-- to be able to accept a request from a client, grab the JSON from the body, and turn that
-- JSON value to a Command of the Order Taking service. The 'handleCommand' is the real
-- entry point for the real abstraction.

handleCommand : Command.OrderTaking -> Promise (Either Error.OrderTaking Event.OrderTaking)
handleCommand = boundedContext orderTakingContext

-- Scaffolding of getting the JSON and the Command out of the request, execute the command in
-- the OrderTaking Bounded Context and render the result JSON from the Event data coming
-- from the Bounded Context.
orderTakingHandler : Request -> Response -> IO ()
orderTakingHandler req rsp =
  resolve' (\_ => pure ()) (\err => putStrLn $ "Unhandled error has happened: \{err}") $ do
    -- Extract data from request.
    rsp.setHeader "Content-Type" "application/json"
    rsp.setHeader "Access-Control-Allow-Origin" "*"
    content <- req.body
    putStrLn content
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
    -- Execute the learnt command.
    result <- handleCommand cmd
    -- Render the result response.
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
  server <- http.createServer orderTakingHandler
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
