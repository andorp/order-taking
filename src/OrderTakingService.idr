module OrderTakingService

import Language.JSON
import Data.String
import System

import OrderTaking.Domain.PlaceOrder
import OrderTaking.BoundedContext.PlaceOrder
import OrderTaking.Database.Order
import OrderTaking.Database.Product
import OrderTaking.Domain.Backend
import OrderTaking.DTO.PlaceOrder

import Rango.BoundedContext.Workflow
import Rango.DataTransfer.JSON.Interfaces

import Service.NodeJS.HTTP
import Service.NodeJS.SQLite
import Service.NodeJS.MD5
import Service.NodeJS.Date
import Service.NodeJS.Promise


orderTaking : RunBackend -> Request -> Response -> IO ()
orderTaking rb req rsp = do
  resolve (do
    content <- Request.body req
    putStrLn content
    Response.setHeader rsp "Content-Type" "application/json"
    Response.setHeader rsp "Access-Control-Allow-Origin" "*"
    let Just jsValue = JSON.parse content
        | Nothing => do
            putStrLn "Couldn't parse incoming JSON."
            Response.statusCode rsp 400
            Response.end rsp "{\"message\":\"Couldn't parse incoming JSON.\"}"
    let Just orderFormDTO = the (Maybe OrderFormDTO) (fromJSON jsValue)
        | Nothing => do
            putStrLn "Couldn't parse JSValue."
            Response.statusCode rsp 400
            Response.end rsp "{\"message\":\"Couldn't parse DTO JSON.\"}"
    orderEvents <- runBackend rb $ orderTakingWorkflow $ FromUpstream.orderForm orderFormDTO
    case orderEvents of
      Left err => do
        putStrLn "There was an error: \{show err}"
        Response.statusCode rsp 400
        Response.end rsp $ "{ \"message\": \"There was an error: " ++ show err ++ "\"}"
      Right events => do
        putStrLn $ show events
        Response.statusCode rsp 200
        Response.end rsp $ "{ \"message\": \"Your order has taken!\" }"
    pure ())
    (\_ => pure ())
    (\err => do
      -- Response.end rsp $ "There was an error: " ++ show err
      putStrLn err)
  where
    orderTakingWorkflow : OrderForm -> Backend (List PlacedOrderEvent)
    orderTakingWorkflow orderForm = interpret backend $ morph withPOMMapping PlaceOrder.workflow $ orderForm

fourOfour : Request -> Response -> IO ()
fourOfour req rsp = do
  Response.setHeader  rsp "Content-Type" "plain/text"
  Response.statusCode rsp 404
  Response.end        rsp "Not found."

dispatcher : RunBackend -> Request -> Response -> IO ()
dispatcher runBackend req rsp = do
  case Request.url req of
    "/"           => orderTaking runBackend req rsp
    _             => fourOfour req rsp

startService : IO ()
startService = do
  putStrLn "Staring Order taking service."
  let orderDBComp = orderDBSQLite
  let productDBComp = productDBSQlite
  run <- mkRunBackend
  http   <- HTTP.require
  server <- HTTP.createServer http (dispatcher run)
  Server.listen server 3000 "127.0.0.1"

initDB : IO ()
initDB = do
  Database.Order.initDB
  Database.Product.initDB

main : IO ()
main = do
  args <- getArgs
  printLn args
  if "--init-db" `elem` args
    then OrderTakingService.initDB
    else startService
