module Service.NodeJS.Date

-- Foreign function interface for NodeJS backend.
-- After 'node:lambda:' there should be a JavaScript snippet,
-- which will be injected into the generated JavaScript.
%foreign "node:lambda: u => Number(new Date().getTime())"
ffi_now : () -> PrimIO Int

-- With HasIO interface any IO can be lifted to the Monad
-- which implements this interface.
--
-- primIO creates an IO action from PrimIO. Only PrimIO
-- can be used in foreign interface if IO like computation
-- is represented by the foreign function.
export
now : HasIO io => io Int
now = primIO (ffi_now ())
