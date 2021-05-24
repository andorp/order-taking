module OrderTaking.DTO.PlaceOrder

public export
record AddressDTO where
  constructor MkAddressDTO
  addressLine1 : String
  addressLine2 : Maybe String
  addressLine3 : Maybe String
  addressLine4 : Maybe String
  city         : String
  zipCode      : String
