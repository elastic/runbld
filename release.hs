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

writeV :: Text -> Text -> IO ()
writeV f v = writeFile (T.unpack f) $ (T.unpack v) ++ "\n"

git :: Text -> [Text] -> IO ExitCode
git cmd args = proc "git" (cmd:args) empty

make :: Text -> IO ExitCode
make target = proc "make" [target] empty

upload :: Text -> Text -> IO ExitCode
upload ver loc = proc "s3cmd" [ "put", "-P"
               , (T.append "target/runbld-" ver)
               , (T.append "s3://" loc)
               ] empty

main = do
  let vfile = T.pack "resources/version.txt"
  (version, dest) <- options "Release runbld" parser
  writeV vfile version
  git "add" [vfile]
  git "commit" ["-m", version]
  make "package"
  git "tag" ["-am", version, version]
  upload version dest

