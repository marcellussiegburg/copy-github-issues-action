{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Main where

import qualified Data.ByteString.Char8            as BS (pack)
import qualified Data.Map                         as M
import qualified Data.Text                        as T (Text, pack, replace)
import qualified Data.Vector                      as V (empty, head, null)
import qualified GitHub                           as GH

import Control.Monad                    (void, when)
import Data.Char                        (toLower, toUpper)
import Data.Maybe                       (fromMaybe)
import Data.String.Interpolate          (i)
import Data.String.Interpolate.Conversion
  (Interpolatable)
import Network.HTTP.Client
  (HttpException (..), HttpExceptionContent (..), responseStatus)
import Network.HTTP.Types               (statusCode)
import System.Environment               (getEnvironment)

name :: String -> GH.Name entity
name = GH.mkName undefined . T.pack

data Config = Config {
  sourceOwner       :: String,
  sourceToken       :: GH.Auth,
  sourceRepo        :: String,
  targetOwner       :: String,
  targetToken       :: GH.Auth,
  targetRepo        :: String,
  forceNumber       :: Bool,
  copyAll           :: Bool,
  closeIssue        :: Bool,
  referenceOld      :: Bool,
  issueTitle        :: Maybe T.Text,
  issueBody         :: Maybe T.Text,
  deletedIssueTitle :: Maybe T.Text,
  deletedIssueBody  :: Maybe T.Text
  } deriving Show

getConfig :: M.Map String String -> IO Config
getConfig env =
  let sToken   = required "INPUT_SOURCE_TOKEN"
      (owner, repo) = splitAtFirst '/' $ required "GITHUB_REPOSITORY"
      sUser    = optional "INPUT_SOURCE_USER"
      sOwner   = fromMaybe owner $ optional "INPUT_SOURCE_OWNER"
      sRepo    = fromMaybe repo $ optional "INPUT_SOURCE_REPOSITORY"
      tUser    = optional "INPUT_TARGET_USER"
      tOwner   = fromMaybe sOwner $ optional "INPUT_TARGET_USER"
      tRepo    = fromMaybe sRepo $ optional "INPUT_TARGET_REPOSITORY"
      tToken   = fromMaybe sToken $ optional "INPUT_TARGET_TOKEN"
      forceNIs = maybe True readCapital $ optional "INPUT_FORCE_ISSUE_NUMBER"
      copyIs   = maybe False readCapital $ optional "INPUT_COPY_ALL_MISSING_ISSUES"
      closeI   = maybe False readCapital $ optional "INPUT_CLOSE_ISSUE"
      refOld   = maybe False readCapital $ optional "INPUT_REFERENCE_OLD_ISSUE"
      iTitle   = fmap T.pack $ optional "INPUT_ISSUE_TITLE"
      iBody    = fmap T.pack $ optional "INPUT_ISSUE_BODY"
      diTitle  = fmap T.pack $ optional "INPUT_DELETED_ISSUE_TITLE"
      diBody   = fmap T.pack $ optional "INPUT_DELETED_ISSUE_BODY"
  in return $ Config {
    sourceOwner       = sOwner,
    sourceToken       = authMethod sUser $ BS.pack sToken,
    sourceRepo        = sRepo,
    targetOwner       = tOwner,
    targetToken       = authMethod tUser $ BS.pack tToken,
    targetRepo        = tRepo,
    forceNumber       = forceNIs,
    copyAll           = copyIs,
    closeIssue        = closeI,
    referenceOld      = refOld,
    issueTitle        = iTitle,
    issueBody         = iBody,
    deletedIssueTitle = diTitle,
    deletedIssueBody  = diBody
    }
  where
    required k = fromMaybe (error [i|#{k} is required|]) $ lookFor k env
    optional k = lookFor k env
    lookFor  k = look . M.lookup k
      where
        look Nothing   = Nothing
        look (Just "") = Nothing
        look (Just v ) = Just v
    authMethod = maybe GH.OAuth (GH.BasicAuth . BS.pack)

main :: IO ()
main = do
  env    <- M.fromList <$> getEnvironment
  config <- getConfig env
  when (sourceOwner config == targetOwner config
        && sourceRepo config == targetRepo config)
    $ putStrLn "Warning: source and target owner or source and target repo should differ"
  issue <- either (error . show) V.head <$> GH.github
    (sourceToken config)
    GH.issuesForRepoR
    (name $ sourceOwner config)
    (name $ sourceRepo config)
    GH.optionsIrrelevantAssignee
    (GH.FetchAtLeast 1)
  if forceNumber config
    then withForceNumber config issue
    else do
    when (copyAll config)
      $ putStrLn "Warning: not copying all old issues as force issue number is disabled"
    createIssue
      config
      (GH.issueNumber issue)
      (GH.issueTitle issue)
      (GH.issueBody issue)
      False

createIssue :: Config -> GH.IssueNumber -> T.Text -> Maybe T.Text -> Bool -> IO ()
createIssue config nr title body deleted = do
  let title' = insertTitle iTitle
  let body'  = insertBody iBody
  let new = GH.NewIssue title' body' V.empty Nothing Nothing
  issue' <- either (error . show) id <$> GH.github
    (targetToken config)
    GH.createIssueR
    (name $ targetOwner config)
    (name $ targetRepo config)
    new
  when (referenceOld config) $ void $ GH.github
    (targetToken config)
    GH.createCommentR
    (name $ targetOwner config)
    (name $ targetRepo config)
    (GH.issueNumber issue')
    [i|see #{sourceOwner config}/#{sourceRepo config}##{GH.unIssueNumber nr}|]
  when (closeIssue config) $ void $ do
    let close = GH.EditIssue Nothing Nothing Nothing (Just GH.StateClosed) Nothing Nothing
    GH.github
      (targetToken config)
      GH.editIssueR
      (name $ targetOwner config)
      (name $ targetRepo config)
      (GH.issueNumber issue')
      close
  where
    iTitle
      | deleted   = deletedIssueTitle config
      | otherwise = issueTitle config
    iBody
      | deleted   = deletedIssueBody config
      | otherwise = issueBody config
    insertTitle = maybe title $ replaceVariable "issueTitle" title
    insertBody = maybe body
      $ Just . replaceVariable "issueBody" (fromMaybe "" body)

replaceVariable
  :: Interpolatable 'True a T.Text
  => T.Text -> T.Text -> a -> T.Text
replaceVariable var by text = T.replace [i|$(#{var})|] by [i|#{text}|]

withForceNumber :: Config -> GH.Issue -> IO ()
withForceNumber config sourceIssue = do
  targetIssue <- GH.github
    (targetToken config)
    GH.issueR
    (name $ targetOwner config)
    (name $ targetRepo config)
    $ GH.issueNumber sourceIssue
  when (isNotFound targetIssue) $ do
    latestTargetIssues <- either (error . show) id <$> GH.github
      (targetToken config)
      GH.issuesForRepoR
      (name $ targetOwner config)
      (name $ targetRepo config)
      GH.optionsIrrelevantAssignee
      (GH.FetchAtLeast 1)
    let latestTargetIssue =
          if V.null latestTargetIssues
          then 1
          else GH.unIssueNumber $ GH.issueNumber $ V.head latestTargetIssues
    let issuesToCopy = [latestTargetIssue .. GH.unIssueNumber (GH.issueNumber sourceIssue)]
    if copyAll config || length issuesToCopy <= 1
      then copyIssue config `mapM_` issuesToCopy
      else
      putStrLn "Warning: not copying issue as issue numbers are enforced but intermediate issues shall not be created."
  where
    isNotFound (Left (GH.HTTPError (HttpExceptionRequest _ (StatusCodeException r _))))
      | statusCode (responseStatus r) == 404 = True
      | otherwise                            = False
    isNotFound _ = False

copyIssue :: Config -> Int -> IO ()
copyIssue config x = do
  sourceIssue <- GH.github
    (sourceToken config)
    GH.issueR
    (name $ sourceOwner config)
    (name $ sourceRepo config)
    (GH.IssueNumber x)
  if isGone sourceIssue
    then createIssue config (GH.IssueNumber x) "" Nothing True
    else do
    let issue = either (error . show) id sourceIssue
    createIssue
      config
      (GH.issueNumber issue)
      (GH.issueTitle issue)
      (GH.issueBody issue)
      False
  where
    isGone (Left (GH.HTTPError (HttpExceptionRequest _ (StatusCodeException r _))))
      | statusCode (responseStatus r) == 410 = True
      | otherwise                            = False
    isGone _ = False

readCapital :: String -> Bool
readCapital = read . onFirst toUpper . fmap toLower

splitAtFirst :: Eq a => a -> [a] -> ([a], [a])
splitAtFirst _ []     = ([], [])
splitAtFirst s (x:xs)
  | s == x    = ([], xs)
  | otherwise = (x:ys, zs)
  where
    (ys, zs) = splitAtFirst s xs

onFirst :: (a -> a) -> [a] -> [a]
onFirst _ []     = []
onFirst f (x:xs) = f x : xs
