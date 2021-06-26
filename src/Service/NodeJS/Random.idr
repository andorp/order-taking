module Service.NodeJS.Random

%foreign "node:lambda: u => (Math.random())"
ffi_random : () -> PrimIO Double

||| Generate a random Double between 0.0 and 1.0
export
double : HasIO io => io Double
double = primIO $ ffi_random ()
