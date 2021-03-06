{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Aftok.QConfig where


import           Control.Lens                   ( makeLenses
                                                , (^.)
                                                )
import qualified Data.ByteString.Char8         as C8
import qualified Data.Configurator             as C
import qualified Data.Configurator.Types       as CT
import qualified Data.List                     as L
import           System.Environment             ( getEnvironment )
import           Filesystem.Path.CurrentOS      ( fromText
                                                , encodeString
                                                )
import qualified Filesystem.Path.CurrentOS     as P

import           Snap.Core
import qualified Snap.Http.Server.Config       as SC
import           Snap.Snaplet.PostgresqlSimple

import           Aftok.Config

data QConfig = QConfig
  { _hostname      :: C8.ByteString
  , _port          :: Int
  , _authSiteKey   :: P.FilePath
  , _cookieTimeout :: Maybe Int
  , _pgsConfig     :: PGSConfig
  , _smtpConfig    :: SmtpConfig
  , _billingConfig :: BillingConfig
  , _templatePath  :: P.FilePath
  , _staticAssetPath :: P.FilePath
  }
makeLenses ''QConfig

loadQConfig :: P.FilePath -> IO QConfig
loadQConfig cfgFile = do
  env <- getEnvironment
  cfg <- C.load [C.Required $ encodeString cfgFile]
  let dbEnvCfg = pgsDefaultConfig . C8.pack <$> L.lookup "DATABASE_URL" env
  readQConfig cfg dbEnvCfg

readQConfig :: CT.Config -> Maybe PGSConfig -> IO QConfig
readQConfig cfg pc =
  QConfig
    <$> C.lookupDefault "localhost" cfg "hostname"
    <*> C.lookupDefault 8000 cfg "port"
    <*> (fromText <$> C.require cfg "siteKey")
    <*> C.lookup cfg "cookieTimeout"
    <*> maybe (mkPGSConfig $ C.subconfig "db" cfg) pure pc
    <*> readSmtpConfig cfg
    <*> (readBillingConfig $ C.subconfig "billing" cfg)
    <*> (fromText <$> C.lookupDefault "/opt/aftok/server/templates/"
                                      cfg
                                      "templatePath"
        )
    <*> (fromText <$> C.lookupDefault "/opt/aftok/server/static/"
                                      cfg
                                      "staticAssetPath"
        )

baseSnapConfig :: QConfig -> SC.Config m a -> SC.Config m a
baseSnapConfig qc = SC.setHostname (qc ^. hostname) . SC.setPort (qc ^. port)

-- configuration specific to Snap, commandLineConfig arguments override
-- config file.
snapConfig :: QConfig -> IO (SC.Config Snap a)
snapConfig qc = SC.commandLineConfig $ baseSnapConfig qc SC.emptyConfig

