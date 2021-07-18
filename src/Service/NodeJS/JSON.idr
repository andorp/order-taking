module Service.NodeJS.JSON

import public Language.JSON
import Data.List


public export
data JSON : Type where [external]

%foreign "node:lambda: r => {return JSON.stringify(r);}"
ffi_stringify : NodeJS.JSON.JSON -> PrimIO String

export
stringify : HasIO io => NodeJS.JSON.JSON -> io String
stringify r = primIO (ffi_stringify r)

export
convert : HasIO io => NodeJS.JSON.JSON -> io (Maybe Data.JSON)
convert o = do
  jsonStr <- stringify o
  -- This is slow, we need a better JSON API
  pure $ JSON.parse jsonStr

infixl 9 .:

export
(.:) : Data.JSON -> Lazy String -> Maybe Data.JSON
(JObject xs) .: field = lookup field xs
o            .: _     = Nothing