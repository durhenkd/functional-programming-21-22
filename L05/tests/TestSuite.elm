module TestSuite exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import L05 exposing(..)

import Exercises exposing (..)
import Theme

suite : Test
suite = 
  describe "Exercises module"
    [ describe "Exercises.countVowels"
      [ test "Test One" <|
        \_ -> Expect.equal 4 <| countVowels "Tiberiu"
        , test "Test Two" <|
        \_ -> Expect.equal 0 <| countVowels "BCdgthlMnq"]
      , describe "Exercises.usersWithPhoneNumbers"
      [ test "First example" <| 
          \_ -> 
            let 
              user1 = makeUser "john00" "johndoe@gmail.com" "John" "Doe" (Just "0123456789")
              user2 = makeUser "jane12" "jane12@yahoo.com" "Jane" "Doe" Nothing
              user3 = makeUser "jacob14" "jacobh@yahoo.com" "Jacob" "Hunt" (Just "345870481")
            in
              Expect.equal [user1.email, user3.email] <| usersWithPhoneNumbers [user1, user2, user3]
      , test "My Test - Empty" <| 
          \_ -> 
            let 
              user1 = makeUser "john00" "johndoe@gmail.com" "John" "Doe" Nothing
              user2 = makeUser "jane12" "jane12@yahoo.com" "Jane" "Doe" Nothing
              user3 = makeUser "jacob14" "jacobh@yahoo.com" "Jacob" "Hunt" Nothing
            in
              Expect.equal [] <| usersWithPhoneNumbers [user1, user2, user3]
      ]
    , describe "Exercises.changePreferenceToDarkTheme"
      [ test "First example" <| 
          \_ -> 
            let 
              config1 = AccountConfiguration Theme.Light True False
              config1Dark = AccountConfiguration Theme.Dark True False
              config2 = AccountConfiguration Theme.Dark False False
              config3 = AccountConfiguration Theme.Dark False True
            in
              Expect.equal [config1Dark, config2, config3] <| changePreferenceToDarkTheme [config1, config2, config3]
      , test "My Test - Identical" <|
        \_ -> 
          let 
              config1 = AccountConfiguration Theme.Dark True False
              config2 = AccountConfiguration Theme.Dark False False
              config3 = AccountConfiguration Theme.Dark False True
            in
              Expect.equal [config1, config2, config3] <| changePreferenceToDarkTheme [config1, config2, config3]

      ]
    , describe "Excercices.chunks"
      [
        test "Chunk Test One" <|
        \_ -> Expect.equal [[1,2],[3,4],[5,6]] <| chunks 2 [1,2,3,4,5,6]
      , test "Chunk Test Two" <|
        \_ -> Expect.equal [[1,2,3],[4]] <| chunks 3 [1,2,3,4]
      , test "Chunk Test Empty" <|
        \_ -> Expect.equal [] <| chunks 4 []
      , test "My Test 1" <|
        \_ -> Expect.equal [[1,2,3]] <| chunks 4 [1,2,3]
      , test "My Test 2" <|
        \_ -> Expect.equal [[1,2,3,4], [5,6]] <| chunks 4 [1,2,3,4,5,6]
      ]
    ]