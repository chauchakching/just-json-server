module JustJsonServer where

import Control.Applicative (Alternative ((<|>)))
import Data.Aeson (Array, FromJSON (..), Object, Result (..), Value (..), decode, eitherDecode, encode, withObject, (.:))
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Aeson.TH (defaultOptions, deriveJSON)
import qualified Data.Aeson.Types as AT
import qualified Data.ByteString.Lazy as LB
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as C
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HMap
import qualified Data.Map as Map
import qualified Data.Text as T
import Data.Text.Lazy.Encoding (encodeUtf8)
import GHC.Generics (Generic)
import Network.HTTP.Types (Method, methodDelete, methodGet, methodPatch, methodPost, methodPut, status200, status201, status400, status404)
import Network.Wai (Application, Request (pathInfo, requestBody, requestMethod), Response, ResponseReceived, responseLBS, strictRequestBody)
import Network.Wai.Handler.Warp (run)
import Options.Applicative (argument, execParser, fullDesc, header, helper, info, metavar, progDesc, str, (<**>))
import qualified Options.Applicative as OA
import RIO.List (find, findIndex, headMaybe)
import qualified RIO.List as L
import RIO.Prelude (fromMaybe)
import System.Directory (getCurrentDirectory)
import System.FilePath (joinPath)

newtype CommandArgs = CommandArgs
  { jsonFilename :: T.Text
  }
  deriving (Show)

newtype Model = Model (HashMap T.Text [Object])
  deriving (Show, Generic)

$(deriveJSON defaultOptions ''Model)

runJustJsonServer :: IO ()
runJustJsonServer = do
  putStrLn "starting json server..."
  let opts = info (argsParser <**> helper) (fullDesc <> progDesc "hey there" <> header "json server")
  commandArgs <- execParser opts
  jsonResources <- checkJsonResources commandArgs
  let app req respond = do
        putStrLn $ "received request" ++ show req
        putStrLn $ "request path" ++ show (pathInfo req)
        let (method, entity, entityId) = (requestMethod req, (!!? 0) $ pathInfo req, (!!? 1) $ pathInfo req)
        body <- strictRequestBody req
        respond =<< handler commandArgs method body entity entityId
  run 3000 app

handler :: CommandArgs -> Method -> ByteString -> Maybe T.Text -> Maybe T.Text -> IO Response
handler commandArgs _ _ Nothing _ = return $ responseLBS status200 [] "Hello World"
-- GET many
handler commandArgs method _ (Just resource) Nothing = do
  resourceData <- getResourceData resource <$> readJsonData (jsonFilename commandArgs)
  return $ responseLBS status200 [] $ encode resourceData
handler commandArgs method body (Just resource) (Just reqResourceId)
  -- GET
  | method == methodGet = do
    resourceData <- getResourceData resource <$> readJsonData (jsonFilename commandArgs)
    let maybeSelectedDatum = find (byObjectId reqResourceId) resourceData
    case maybeSelectedDatum of
      Nothing -> return response404
      Just selectedDatum -> return $ responseLBS status200 [] $ encode selectedDatum
  -- POST
  | method == methodPost = do
    model <- readJsonData (jsonFilename commandArgs)
    let resourceData = getResourceData resource model
    let existingObject = find (byObjectId reqResourceId) resourceData
    case existingObject of
      Just _ -> return $ response400DuplicatedId reqResourceId
      Nothing -> do
        case decode body of
          Nothing -> return response400InvalidJson
          Just o -> do
            let updatedModel = updateModelResource resource (resourceData ++ [o]) model
            writeJsonData (jsonFilename commandArgs) updatedModel
            return $ responseLBS status201 [] ""
  -- PUT
  | method == methodPatch || method == methodPut = do
    model <- readJsonData (jsonFilename commandArgs)
    let resourceData = getResourceData resource model
    let existingObject = find (byObjectId reqResourceId) resourceData
    case existingObject of
      Nothing -> return response404
      Just _ -> do
        case decode body of
          Nothing -> return response400InvalidJson
          Just o -> do
            let updatedResourceData = replaceOrInsertElem o (byObjectId reqResourceId) resourceData
            let updatedModel = updateModelResource resource updatedResourceData model
            writeJsonData (jsonFilename commandArgs) updatedModel
            return $ responseLBS status200 [] $ encode o
  -- DELETE
  | method == methodDelete = do
    model <- readJsonData (jsonFilename commandArgs)
    let resourceData = getResourceData resource model
    let originalLength = length resourceData
    let filteredData = filter (byObjectId reqResourceId) resourceData

    if length filteredData == originalLength
      then return response404
      else do
        let updatedModel = updateModelResource resource filteredData model
        writeJsonData (jsonFilename commandArgs) updatedModel
        return $ responseLBS status200 [] ""
  | otherwise = return response404

replaceOrInsertElem :: a -> (a -> Bool) -> [a] -> [a]
replaceOrInsertElem x f xs = firstHalf ++ [x] ++ drop 1 secondHalf
  where
    (firstHalf, secondHalf) = splitAt (fromMaybe (length xs) maybeIndex) xs
    maybeIndex = findIndex f xs

response404 :: Response
response404 = responseLBS status404 [] "Not Found"

response400InvalidJson :: Response
response400InvalidJson = responseLBS status400 [] "invalid json"

response400DuplicatedId :: T.Text -> Response
response400DuplicatedId objectId = responseLBS status400 [] $ C.pack $ "object with id " ++ show objectId ++ " already exists"

byObjectId :: T.Text -> Object -> Bool
byObjectId objectId = (Just objectId ==) . getObjectId

-- Handle number or string id
getObjectId :: Object -> Maybe T.Text
getObjectId o = maybeText <|> T.pack . show <$> maybeInt
  where
    maybeText = AT.parseMaybe (.: "id") o :: Maybe T.Text
    maybeInt = AT.parseMaybe (.: "id") o :: Maybe Int

getResourceData :: T.Text -> Model -> [Object]
getResourceData resource (Model hashmap) = HMap.lookupDefault [] resource hashmap

checkJsonResources :: CommandArgs -> IO [T.Text]
checkJsonResources args = do
  putStrLn $ "command arguments: " ++ show args

  filePath <- getTargetFilePath $ jsonFilename args
  content <- LB.readFile filePath
  let maybeModel = decode content :: Maybe Model
  case maybeModel of
    Nothing -> error "invalid json data"
    Just (Model hashmap) -> do
      let resources = HMap.keys hashmap
      putStrLn $ "resources: " ++ show resources
      return resources

updateModelResource :: T.Text -> [Object] -> Model -> Model
updateModelResource resource resourceData (Model hashmap) = Model updatedHashmap
  where
    updatedHashmap = HMap.insert resource resourceData hashmap

readJsonData :: T.Text -> IO Model
readJsonData filename = do
  filePath <- getTargetFilePath filename
  content <- LB.readFile filePath
  let maybeModel = decode content :: Maybe Model
  case maybeModel of
    Nothing -> error "invalid json data"
    Just model -> return model

writeJsonData :: T.Text -> Model -> IO ()
writeJsonData filename model = do
  filePath <- getTargetFilePath filename
  LB.writeFile filePath $ encodePretty model

argsParser :: OA.Parser CommandArgs
argsParser =
  CommandArgs
    <$> argument str (metavar "JSON_FILE")

getTargetFilePath :: T.Text -> IO FilePath
getTargetFilePath filename = do
  currentDirectory <- getCurrentDirectory
  return $ joinPath [currentDirectory, T.unpack filename]

(!!?) :: [a] -> Int -> Maybe a
(!!?) xs n = L.headMaybe $ L.drop n xs