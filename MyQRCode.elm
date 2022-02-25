module MyQRCode exposing (..)

import Browser
import Html
import Html.Extra as Html
import Html.Attributes as Attr
import Html.Events as Ev
import QRCode

--import Debug exposing (log)

-- Enum
type ContactType = Short | Full

-- Objects
type alias Model = {
  code: Maybe String
  , data: Contacts
  }

type alias Contacts = {
  private: Contact
  , lyss: Contact
  , cu: Contact
  }

type alias Contact = {
  comp: Maybe String
  , street: String
  , city: String
  , postcode: String
  , mobile: Maybe String
  , home: Maybe String
  , work: Maybe String
  , fax: Maybe String
  , email: String
  }

-- Model
initialModel: Model
initialModel = {
  code = Nothing
  , data = {
    private = {comp = Nothing, street = "Oppaustr. 6", city = "Schkopau", postcode = "06258", mobile = Just "+49 173 3530135", work = Nothing, home = Just "+49 3461 4564441", fax = Nothing, email = "michael.schaefer@unimer.de"}
    , lyss = {comp = Just "LYSS-IT GmbH", street = "Brautstr. 1-3", city = "Bruchhausen-Vilsen", postcode = "27305", mobile = Nothing, home = Nothing, work = Just "+49 4252 9138148", fax = Just "+49 4252 9138147", email = "schaefer@lyss-it.de"}
    , cu = {comp = Just "Campusunity GmbH", street = "Ober-Rodener Str. 11c", city = "63322", postcode = "Rödermark", mobile = Nothing, home = Nothing, work = Just "+49 4252 9138148", fax = Just "+49 6074 6934061", email = "michael.schaefer@partner.campusunity.de"}
  }
  }

-- Update
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp -> ( model , Cmd.none)
        CloseQRCode -> ( { model | code = Nothing } , Cmd.none)
        SetLyss -> ( { model | code = Just (contactToVCard Full model.data.lyss) } , Cmd.none)
        SetLyssCu -> ( { model | code = Just (contactToVCard Full model.data.cu) } , Cmd.none)
        SetPrivate -> ( { model | code = Just (contactToVCard Full model.data.private) } , Cmd.none)
        SetJustMobile -> ( { model | code = Just (contactToVCard Short model.data.private) } , Cmd.none)

-- Types
type Msg =
  NoOp
  | CloseQRCode
  | SetLyss
  | SetLyssCu
  | SetPrivate
  | SetJustMobile

-- Methods
qrCodeView : String -> Html.Html msg
qrCodeView message =
    QRCode.encode message
        |> Result.map QRCode.toSvg
        |> Result.withDefault (Html.text "Error while encoding to QRCode.")

contactToVCard: ContactType -> Contact -> String
contactToVCard type_ contact =
  let
    kind = if contact.comp == Nothing then "home" else "work"
    phone = if contact.comp == Nothing then contact.home else contact.work
    --label = contact.street ++ "\n" ++ contact.postcode ++ " " ++ contact.city ++ "\nDeutschland"
    comp = case contact.comp of
      Just _ -> "ORG:" ++ Maybe.withDefault "" contact.comp ++ "\n"
      Nothing -> ""
    mobile = case contact.comp of
      Just _ -> ""
      Nothing -> "TEL;TYPE=cell;VALUE=uri:tel:" ++ Maybe.withDefault "" contact.mobile ++ "\n"
    fax = case contact.comp of
      Just _ -> "TEL;TYPE=fax;VALUE=uri:tel:" ++ Maybe.withDefault "" contact.fax ++ "\n"
      Nothing -> ""
    vcard = case type_ of
      Full -> "BEGIN:VCARD\n" ++
        "VERSION:4.0\n" ++
        "N:Schäfer;Michael;;;\n" ++
        "FN:Michael Schäfer\n" ++
        comp ++
        "TEL;TYPE=" ++ kind ++ ";VALUE=uri:tel:" ++ Maybe.withDefault "" phone ++ "\n" ++
        mobile ++
        fax ++
        "ADR;TYPE=" ++ kind ++ ":;;" ++ contact.street ++ ";" ++ contact.city ++ ";;" ++ contact.postcode ++ ";Germany\n" ++
        "EMAIL;TYPE=" ++ kind ++ ":" ++ contact.email ++ "\n" ++
        "END:VCARD"
      Short -> "BEGIN:VCARD\n" ++
        "VERSION:4.0\n" ++
        "N:Schäfer;Michael;;;\n" ++
        "FN:Michael Schäfer\n" ++
        "TEL;TYPE=cell;VALUE=uri:tel:" ++ Maybe.withDefault "" contact.mobile ++ "\n" ++
        "END:VCARD"
  in
    vcard


{-
BEGIN:VCARD
VERSION:4.0
N:Mustermann;Erika;;Dr.;
FN:Dr. Erika Mustermann
ORG:Wikimedia
ROLE:Kommunikation
TITLE:Redaktion & Gestaltung
PHOTO;MEDIATYPE=image/jpeg:http://commons.wikimedia.org/wiki/File:Erika_Mustermann_2010.jpg
TEL;TYPE=work,voice;VALUE=uri:tel:+49-221-9999123
TEL;TYPE=home,voice;VALUE=uri:tel:+49-221-1234567
ADR;TYPE=home;LABEL="Heidestraße 17\n51147 Köln\nDeutschland"
 :;;Heidestraße 17;Köln;;51147;Germany
EMAIL:erika@mustermann.de
REV:20140301T221110Z
END:VCARD
-}



-- View
view : Model -> Html.Html Msg
view model =
  let
    qrCode = case model.code of
      Just str -> Html.div [ Attr.style "display" "inline-block", Ev.onClick CloseQRCode ][ qrCodeView str ]
      Nothing -> Html.nothing
  in
    Html.div[][
      Html.div [][
        Html.input [ Attr.type_ "button", Attr.value "LYSS-IT", Ev.onClick SetLyss ][]
        , Html.input [ Attr.type_ "button", Attr.value "CU", Ev.onClick SetLyssCu ][]
        , Html.input [ Attr.type_ "button", Attr.value "Privat", Ev.onClick SetPrivate ][]
        , Html.input [ Attr.type_ "button", Attr.value "Mobile", Ev.onClick SetJustMobile ][]
      ]
      , qrCode
    ]

main : Program () Model Msg
main =
    Browser.element
        { init = \() -> init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

subscriptions : Model -> Sub Msg
subscriptions _ = Sub.none

init : ( Model, Cmd Msg )
init =  ( initialModel, Cmd.none
  )
