module Json exposing (..)


import Json.Decode as Dec
import Json.Encode as Enc


keyValuePairDecoder : Dec.Decoder (String,String)
keyValuePairDecoder =
    Dec.map2 ( \x y -> (x,y) )
        (Dec.field "key" Dec.string)
        (Dec.field "value" Dec.string)


decodeKeyValuePair : String -> Result String (String,String)
decodeKeyValuePair str =
    Dec.decodeString keyValuePairDecoder str


encodeKeyValuePair : (String,String) -> String
encodeKeyValuePair (key,value) =
    Enc.object
        [ ("key", key |> Enc.string) 
        , ("value", value |> Enc.string)
        ]
    |> Enc.encode 0