{- HLINT ignore "Use newtype instead of data" -}
module PSR.HTTP.API (
    SiteAPI,
    FullAPI,
    EventType (..),
    EventFilterParams (..),
    SiteRoutes (..),
    EventRoutes (..),
    siteOpenAPI,
    fullApi,
    serveOpenApiUI,
) where

import Data.Aeson (FromJSON, ToJSON)
import Data.OpenApi (OpenApi, OpenApiType (..), ToParamSchema (..), ToSchema)
import Data.OpenApi.Lens
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import GHC.Generics (Generic)
import Lens.Micro
import Servant
import Servant.OpenApi
import Servant.QueryParam.OpenApi.Record ()
import Servant.QueryParam.Record (RecordParam)
import Servant.QueryParam.Server.Record ()
import Servant.QueryParam.TypeLevel (DropPrefix, Eval, Exp)
import Servant.Swagger.UI

data Event = Event
    { content :: Text
    }
    deriving (Generic, Show)

instance FromJSON Event
instance ToJSON Event
instance ToSchema Event

data DropPrefixExp :: sym -> Exp sym
type instance Eval (DropPrefixExp sym) = DropPrefix sym

data EventType
    = Execution
    | Selection
    | Cancellation
    deriving (Generic)

instance FromHttpApiData EventType where
    parseQueryParam = \case
        "execution" -> pure Execution
        "selection" -> pure Selection
        "cancellation" -> pure Cancellation
        _ -> Left "Unknown event type"

instance ToParamSchema EventType where
    toParamSchema _ =
        mempty
            & (type_ ?~ OpenApiString)
            & (enum_ ?~ ["execution", "selection", "cancellation"])

data EventFilterParams = EventFilterParams
    { _filterQueryParam_type :: Maybe EventType
    , _filterQueryParam_time_begin :: Maybe UTCTime
    , _filterQueryParam_time_end :: Maybe UTCTime
    , _filterQueryParam_slot_begin :: Maybe Integer
    , _filterQueryParam_slot_end :: Maybe Integer
    , _filterQueryParam_limit :: Maybe Integer
    , _filterQueryParam_offset :: Maybe Integer
    , _filterQueryParam_name :: Maybe Text
    }
    deriving (Generic)

type EventFilterParams' = RecordParam DropPrefixExp EventFilterParams

type AllEventsDocs endpoint =
    Summary "All events"
        :> Description "All events, with optional filters"
        :> endpoint
type NamedEventsDocs endpoint =
    Summary "Look up a specific script"
        :> Description "Events for a specific script by name or hash, as well as filter params"
        :> endpoint

data EventRoutes route = EventRoutes
    { allEvents :: route :- AllEventsDocs (EventFilterParams' :> Get '[JSON] [Event])
    , namedEvents :: route :- NamedEventsDocs (EventFilterParams' :> Capture "script_hash_or_name" Text :> Get '[JSON] [Event])
    }
    deriving (Generic)

data SiteRoutes route = SiteRoutes
    { events :: route :- "events" :> NamedRoutes EventRoutes
    }
    deriving (Generic)

type SiteAPI = NamedRoutes SiteRoutes

siteApi :: Proxy SiteAPI
siteApi = Proxy

type OpenApiUI = SwaggerSchemaUI "swagger-ui" "swagger.json"

serveOpenApiUI :: Server OpenApiUI
serveOpenApiUI = swaggerSchemaUIServer siteOpenAPI

siteOpenAPI :: OpenApi
siteOpenAPI =
    toOpenApi siteApi
        & (info . title .~ "Plutus Script Re-executor")
        & (info . version .~ "1.0") -- TODO: Get this from Cabal
        & (info . description ?~ "HTTP API to interact with the Plutus Script Re-executor")

type FullAPI = SiteAPI :<|> OpenApiUI

fullApi :: Proxy FullAPI
fullApi = Proxy
