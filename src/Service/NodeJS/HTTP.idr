module Service.NodeJS.HTTP

import Data.String


namespace Request

  export
  data Request : Type where [external]

  %foreign "node:lambda: (req, f) => {let data = '';req.on('data',chunk=>{data+=chunk;});req.on('end',()=> f(data););}"
  ffi_body : Request -> (String -> PrimIO ()) -> PrimIO ()

  export
  body : Request -> (String -> IO ()) -> IO ()
  body req f = primIO (ffi_body req (\str => toPrim (f str)))

namespace Response

  export
  data Response : Type where [external]

  %foreign "node:lambda: (r,c) => (r.statusCode = Number(c))"
  ffi_statusCode : Response -> Int -> PrimIO ()

  export
  statusCode : Response -> Int -> IO ()
  statusCode r c = primIO (ffi_statusCode r c)

  %foreign "node:lambda: (r,h,v) => {r.setHeader(h, v)}"
  ffi_setHeader : Response -> String -> String -> PrimIO ()

  export
  setHeader : Response -> String -> String -> IO ()
  setHeader r h v = primIO (ffi_setHeader r h v)

  %foreign "node:lambda: (r,e) => (r.end(e))"
  ffi_end : Response -> String -> PrimIO ()

  export
  end : Response -> String -> IO ()
  end r e = primIO (ffi_end r e)

namespace Server

  export
  data Server : Type where [external]

  %foreign "node:lambda: (s,p,h) => (s.listen(Number(p),h,() => {}))"
  ffi_listen : Server -> Int -> String -> PrimIO ()

  export
  listen : Server -> Int -> String -> IO ()
  listen server port hostname = primIO (ffi_listen server port hostname)

namespace HTTP

  export
  data HTTP : Type where [external]

  %foreign "node:lambda: u => require('http')"
  ffi_require : () -> PrimIO HTTP

  export
  require : IO HTTP
  require = primIO (ffi_require ())

  %foreign "node:lambda: (http, result) => http.createServer((req,rsp) => {return result(req)(rsp)();})"
  ffi_createServer : HTTP -> (Request -> Response -> PrimIO ()) -> PrimIO Server

  export
  createServer : HTTP -> (Request -> Response -> IO ()) -> IO Server
  createServer http mkResult
    = primIO (ffi_createServer http (\req , rsp => toPrim $ mkResult req rsp))

