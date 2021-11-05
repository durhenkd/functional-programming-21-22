
module Exercises exposing (..)

import Theme exposing (ThemeConfig)


type alias UserDetails = 
  { firstName: String
  , lastName: String
  , phoneNumber: Maybe String
  }
type alias User = {id: String, email: String, details: UserDetails}

makeUser id email firstName lastName phoneNumber = 
  User id email (UserDetails firstName lastName phoneNumber)


type alias AccountConfiguration = 
  { preferredTheme: ThemeConfig
  , subscribedToNewsletter: Bool
  , twoFactorAuthOn: Bool
  }


