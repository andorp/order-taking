module Service.NodeJS.Date

%foreign "node:lambda: u => Number(new Date().getTime())"
ffi_now : () -> PrimIO Int

export
now : HasIO io => io Int
now = primIO (ffi_now ())
