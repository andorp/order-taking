module Service.NodeJS.MD5

-- See: https://www.npmjs.com/package/md5

||| JavaScript MD5 object reference.
|||
||| After MD5 is created it can generate MD5 String values
||| encoding strings.
export
data MD5 : Type where [external]

||| FFI require operation for requesting an MD5 JavaScript object.
%foreign "node:lambda: u => require('md5')"
ffi_require : () -> PrimIO MD5

||| Accessing an MD5 object.
export
require : HasIO io => io MD5
require = primIO (ffi_require ())

||| FFI call to generate MD5 string from a normal string value.
%foreign "node:lambda: (m,s) => m(s)"
ffi_generate : MD5 -> String -> PrimIO String

||| Generate an MD5 string from a normal string value.
export
(.generate) : HasIO io => MD5 -> String -> io String
(.generate) m s = primIO (ffi_generate m s)
