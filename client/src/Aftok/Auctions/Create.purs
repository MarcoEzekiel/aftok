module Aftok.Auctions.Create where

import Prelude

import Aftok.Api.Types (APIError)
import Aftok.HTML.Classes as C
import Aftok.Modals.ModalFFI as ModalFFI
import Aftok.Types (System, ProjectId)
import Control.Monad.Trans.Class (lift)
import Data.Either (Either)
import Data.Foldable (any)
import Data.Maybe (Maybe(..))
import DOM.HTML.Indexed.ButtonType (ButtonType(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Core (ClassName(..))
import Halogen.HTML.Events as E
import Halogen.HTML.Properties as P
import Halogen.HTML.Properties.ARIA as ARIA

data Field
  = PidField
  | NameField
  | DescField

derive instance fieldEq :: Eq Field
derive instance fieldOrd :: Ord Field

type CState =
  { projectId :: Maybe ProjectId
  , name :: Maybe String
  , description :: Maybe String
  , fieldErrors :: Array Field
  }

data Query a
  = OpenModal ProjectId a

data Output 
  = AuctionCreated AuctionId

data Action
  = SetName String
  | SetDesc String
  | Save
  | Close

type Slot id
  = H.Slot Query Output id

type AuctionId = String
type Auction = { name :: String, desc :: String } 

type Capability (m :: Type -> Type)
  = { createAuction :: ProjectId -> Auction -> m (Either APIError AuctionId)
    }

modalId :: String
modalId = "createAuction"

component ::
  forall input m.
  Monad m =>
  System m ->
  Capability m ->
  H.Component HH.HTML Query input Output m
component system caps =
  H.mkComponent
    { initialState: const initialState
    , render
    , eval: H.mkEval
        $ H.defaultEval
            { handleAction = handleAction
            , handleQuery = handleQuery
            }
    }
  where
  initialState :: CState
  initialState =
    { projectId: Nothing
    , name : Nothing
    , description : Nothing
    , fieldErrors : []
    }

  render :: forall slots. CState -> H.ComponentHTML Action slots m
  render st = 
    HH.div
      [ P.classes [ C.modal ]
      , P.id_ modalId
      , P.tabIndex (negate 1)
      , ARIA.role "dialog"
      , ARIA.labelledBy (modalId <> "Title")
      , ARIA.hidden "true"
      ]
      [ HH.div
        [ P.classes [C.modalDialog], ARIA.role "document" ]
        [ HH.div
          [ P.classes [C.modalContent] ]
          [ HH.div
            [ P.classes [C.modalHeader] ]
            [ HH.h5 [P.classes [C.modalTitle], P.id_ (modalId <>"Title") ] [HH.text "Create a new billable item"]
            , HH.button
              [ P.classes [ C.close ]
              , ARIA.label "Close"
              , P.type_ ButtonButton
              , E.onClick (\_ -> Just Close)
              ]
              [ HH.span [ARIA.hidden "true"] [HH.text "×"]]
            ]
          , HH.div
            [ P.classes [C.modalBody] ]
            [ HH.form_
              [ formGroup st
                [ NameField ]
                [ HH.label
                  [ P.for "billableName"]
                  [ HH.text "Product Name" ]
                , HH.input
                  [ P.type_ P.InputText
                  , P.classes [ C.formControl, C.formControlSm ]
                  , P.id_ "billableName"
                  , P.placeholder "A name for the product or service you want to bill for"
                  , E.onValueInput (Just <<< SetName)
                  ]
                ]
              , formGroup st
                [ DescField ]
                [ HH.label
                    [ P.for "billableDesc"]
                    [ HH.text "Product Description" ]
                , HH.input
                    [ P.type_ P.InputText
                    , P.classes [ C.formControl, C.formControlSm ]
                    , P.id_ "billableDesc"
                    , P.placeholder "Description of the product or service"
                    , E.onValueInput (Just <<< SetDesc)
                    ]
                ]
            ]
          , HH.div
            [ P.classes [C.modalFooter] ]
            [ HH.button
              [ P.type_ ButtonButton
              , P.classes [ C.btn, C.btnSecondary]
              , E.onClick (\_ -> Just Close)
              ]
              [ HH.text "Close" ]
            , HH.button
              [ P.type_ ButtonButton
              , P.classes [ C.btn, C.btnPrimary ]
              , E.onClick (\_ -> Just Save)
              ]
              [ HH.text "Create billable"]
            ]
          ]
        ]
      ]
    ]
 
  formGroup :: forall i a. CState -> Array Field -> Array (HH.HTML i a) -> HH.HTML i a
  formGroup st fields body =
    HH.div
     [ P.classes [C.formGroup] ]
     (body <> (fieldError st =<< fields))
 
  formCheckGroup :: forall i a.
    { id :: String
    , checked :: Boolean
    , labelClasses :: Array ClassName
    }
    -> (Unit -> Maybe a)
    -> Array (HH.HTML i a)
    -> HH.HTML i a
  formCheckGroup { id, checked, labelClasses } onChange children  =
    HH.div
      [ P.classes [C.formCheck] ]
      [ HH.input
          ([ P.type_ P.InputRadio
          , P.name "recurType"
          , P.classes [C.formCheckInput]
          , P.id_ id
          , E.onClick \_ -> onChange unit
          ] <> (if checked then [P.checked true] else []))
       , HH.label
           [ P.classes ([C.formCheckLabel ] <> labelClasses)
           , P.for id]
           children
       ]
 
  fieldError :: forall i a. CState -> Field -> Array (HH.HTML i a)
  fieldError st field =
    if any (_ == field) st.fieldErrors
       then case field of
            PidField -> err "No project id found; please report an error"
            NameField -> err "The name field is required"
            DescField -> err "The description field is required"
       else []
    where
    err str = 
      [ HH.div_ 
        [ HH.span 
          [ P.classes (ClassName <$> [ "badge", "badge-danger-soft" ]) ] [ HH.text str ] ] 
      ]

  -- we use a query to initialize, since this is a modal that doesn't actually get unloaded.
  handleQuery :: forall slots a. Query a -> H.HalogenM CState Action slots Output m (Maybe a)
  handleQuery = case _ of
    OpenModal pid a -> do
      H.modify_ (\_ -> initialState { projectId = Just pid })
      lift $ system.toggleModal modalId ModalFFI.ShowModal
      pure (Just a)

  handleAction :: forall slots. Action -> H.HalogenM CState Action slots Output m Unit
  handleAction = case _ of
      SetName name ->
        H.modify_ (_ { name = Just name })
      SetDesc desc ->
        H.modify_ (_ { description = Just desc })
      Save -> do
        pure unit
      Close -> do
        H.modify_ (const initialState) -- wipe the state for safety
        lift $ system.toggleModal modalId ModalFFI.HideModal

-- apiCapability :: Capability Aff
-- apiCapability =
--   { createAuction: createAuction
--   }
-- 
-- mockCapability :: Capability Aff
-- mockCapability =
--   { createAuction: \_ _ -> pure $ Left Forbidden }

