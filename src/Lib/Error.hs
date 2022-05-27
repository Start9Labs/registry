{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE QuasiQuotes     #-}

module Lib.Error where

import           Startlude                      ( (.)
                                                , Eq
                                                , ExceptT
                                                , Exception
                                                , ExitCode
                                                , Show
                                                , Text
                                                , show
                                                )

import           Data.String.Interpolate.IsString
                                                ( i )
import           Network.HTTP.Types             ( Status
                                                , status400
                                                , status404
                                                , status500
                                                )
import           Yesod.Core                     ( (.=)
                                                , ToContent(..)
                                                , ToJSON(toJSON)
                                                , ToTypedContent(..)
                                                , Value(String)
                                                , object
                                                )

type S9ErrT m = ExceptT S9Error m

data S9Error =
      PersistentE !Text
    | AppMgrE !Text !ExitCode
    | NotFoundE !Text
    | InvalidParamsE !Text !Text
    | AssetParseE !Text !Text
    deriving (Show, Eq)

instance Exception S9Error

-- | Redact any sensitive data in this function
toError :: S9Error -> Error
toError = \case
    PersistentE t              -> Error DATABASE_ERROR t
    AppMgrE cmd code           -> Error APPMGR_ERROR [i|"appmgr #{cmd}" exited with #{code}|]
    NotFoundE e                -> Error NOT_FOUND [i|#{e}|]
    InvalidParamsE e     m     -> Error INVALID_PARAMS [i|Could not parse request parameters #{e}: #{m}|]
    AssetParseE    asset found -> Error PARSE_ERROR [i|Could not parse #{asset}: #{found}|]

data ErrorCode =
      DATABASE_ERROR
    | APPMGR_ERROR
    | NOT_FOUND
    | INVALID_PARAMS
    | PARSE_ERROR
    deriving (Eq, Show)
instance ToJSON ErrorCode where
    toJSON = String . show

data Error = Error
    { errorCode    :: !ErrorCode
    , errorMessage :: !Text
    }
    deriving (Eq, Show)
instance ToJSON Error where
    toJSON Error {..} = object ["code" .= errorCode, "message" .= errorMessage]
instance ToContent Error where
    toContent = toContent . toJSON
instance ToTypedContent Error where
    toTypedContent = toTypedContent . toJSON

instance ToTypedContent S9Error where
    toTypedContent = toTypedContent . toJSON . toError
instance ToContent S9Error where
    toContent = toContent . toJSON . toError

toStatus :: S9Error -> Status
toStatus = \case
    PersistentE _      -> status500
    AppMgrE _ _        -> status500
    NotFoundE _        -> status404
    InvalidParamsE _ _ -> status400
    AssetParseE    _ _ -> status500
