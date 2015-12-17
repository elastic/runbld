#!/usr/bin/env runhaskell
{-# LANGUAGE OverloadedStrings #-}

-- Needs idempotency, namely check for existing git tag before doing
-- anything else.

import qualified Data.Text as T
import Turtle
import Prelude hiding (FilePath)

parser :: Parser (Text, Text)
parser = (,) <$> argText "version" "The releaseable version"
             <*> argText "dest" "S3 bucket/keyprefix"

procOrDie :: Text -> [Text] -> IO ()
procOrDie cmd args = do
  echo $ format ("running: "%s%" "%s) cmd (T.intercalate " " args)
  ret <- proc cmd args empty
  case ret of
    ExitSuccess -> return ()
    ExitFailure err -> die (cmd <> " has failed: " <> (repr err))

writeV :: Text -> Text -> IO ()
writeV f v = writeFile (T.unpack f) $ (T.unpack v) ++ "\n"

git :: Text -> [Text] -> IO ()
git cmd args = procOrDie "git" (cmd:args)

make :: Text -> IO ()
make target = procOrDie "make" [target]

upload :: Text -> Text -> IO ()
upload ver loc =
  if loc == (T.last loc) then
    procOrDie "s3cmd" [ "put", "-P"
                      , (T.append "target/runbld-" ver)
                      , (T.append "s3://" loc)
                      ]
  else
    die ("\"" <> loc <> "\" should end in a slash")

main = do
  let vfile = T.pack "resources/version.txt"
  (version, dest) <- options "Release runbld" parser
  writeV vfile version
  git "add" [vfile]
  git "commit" ["-m", version]
  make "package"
  git "tag" ["-am", version, version]
  upload version dest
  git "push" ["--tags"]
