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


helloWorld : Request -> Response -> IO ()
helloWorld req rsp = do
  Response.statusCode rsp 200
  Response.setHeader  rsp "Content-Type" "text/plain"
  Response.end        rsp "Your order has taken! 9"

main1 : IO ()
main1 = do
  putStrLn "Staring Order taking service."
  http   <- HTTP.require
  server <- HTTP.createServer http helloWorld
  Server.listen server 3000 "127.0.0.1"

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

main2 : IO ()
main2 = do
  putStrLn "Starting SQLite test."
  sqlite <- SQLite.require
  db     <- SQLite.database sqlite "ordertaking.db"
  let createUserTable = "CREATE TABLE user (id INTEGER PRIMARY KEY AUTOINCREMENT,name text,email text UNIQUE,password text,CONSTRAINT email_unique UNIQUE (email))"
  Database.run db createUserTable printError
  let insertUser = "INSERT INTO user (name, email, password) VALUES (\"admin\",\"admin@ex.com\",\"passwd\")"
  Database.run db insertUser printError
  let selectUser = "SELECT * FROM user;"
  Database.each db selectUser onRow onCompleted
  Database.close db

main3 : IO ()
main3 = do
  putStrLn "Start MD5 test."
  md5 <- MD5.require
  putStrLn $ !(MD5.create md5 "message")

main4 : IO ()
main4 = do
  Database.Order.initDB
  Database.Product.initDB

main5 : IO ()
main5 = do
  n1 <- Date.now
  printLn n1
  n2 <- Date.now
  printLn n2

main : IO ()
main = main1
