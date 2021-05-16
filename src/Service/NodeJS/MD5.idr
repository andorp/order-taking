module Service.NodeJS.MD5

export
data MD5 : Type where [external]

%foreign "node:lambda: u => require('md5')"
ffi_require : () -> PrimIO MD5

export
require : IO MD5
require = primIO (ffi_require ())

%foreign "node:lambda: (m,s) => m(s)"
ffi_create : MD5 -> String -> PrimIO String

export
create : MD5 -> String -> IO String
create m s = primIO (ffi_create m s)

