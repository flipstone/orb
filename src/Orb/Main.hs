module Orb.Main
  ( main
  , mainParserInfo
  , mainParser
  , mainParserWithCommands
  , openApiLabelArgument
  , generateOpenApiCommand
  , generateOpenApiMain
  ) where

import Data.Aeson.Encode.Pretty qualified as AesonPretty
import Data.ByteString.Lazy qualified as LBS
import Data.List qualified as List
import Options.Applicative qualified as Opt
import System.Exit qualified as Exit
import System.IO qualified as IO

import Orb.OpenApi qualified as OpenApi

{- |
  Constructs a main function that parses the command line arguments and
  provides two subcommands in the executable:

  - @api@ - runs the IO action provided as the main. Presumably this
    invokes some form of 'Orb.Wai.runOrb' (or otherwise runs a way server)
    that serves an application handling the routes provided to this function.

  - @generate-open-api@ - accepts a argument matching one of the labels
    provided to 'OpenApi.provideOpenApi' and prints an OpenApi description
    of the labeled routes as JSON to stdout.
-}
main :: OpenApi.OpenApiRouter a -> IO () -> IO ()
main routes appMain = do
  io <- Opt.customExecParser parserPrefs (mainParserInfo appMain routes)
  io

{- |
  Constructs a 'Opt.ParserInfo' that will execute as description in 'main', but
  can be used as an argument to 'Opt.command' to use it as a subcommand.
-}
mainParserInfo :: IO () -> OpenApi.OpenApiRouter a -> Opt.ParserInfo (IO ())
mainParserInfo apiMain routes =
  Opt.info (mainParser apiMain routes) mempty

{- |
  Constructs a 'Opt.Parser' that will execute as description in 'main', but
  can be used directly in other option parsers.
-}
mainParser :: IO () -> OpenApi.OpenApiRouter a -> Opt.Parser (IO ())
mainParser apiMain =
  mainParserWithCommands
    [ Opt.command "api" (Opt.info (pure apiMain) mempty)
    ]

{- |
  Constructs an 'Opt.Parser' than has the @generate-open-api@ command as in
  the 'main' function along with the other commands passed. In this case no
  @api@ command is added by orb. It is up to the application to passed
  whatever set of other commands it desires.
-}
mainParserWithCommands ::
  [Opt.Mod Opt.CommandFields (IO ())] ->
  OpenApi.OpenApiRouter a ->
  Opt.Parser (IO ())
mainParserWithCommands commands routes =
  Opt.hsubparser $
    mconcat
      ( generateOpenApiCommand routes
          : commands
      )

{- |
  Constructs an 'Opt.command' modifier for the @generate-open-api@ command that
  can be used along with 'Opt.hsubparser' include the command wherever the user
  chooses in their options parsing.
-}
generateOpenApiCommand :: OpenApi.OpenApiRouter a -> Opt.Mod Opt.CommandFields (IO ())
generateOpenApiCommand routes =
  Opt.command "generate-open-api" (Opt.info (generateOpenApiMain routes <$> openApiLabelArgument routes) mempty)

{- |
  Constructs a 'Opt.Parser' than will parse an argument representing one
  of the sections of the provided router that have been labeled via
  'OpenApi.provideOpenApi'. The available labels in the router will be
  used to provide autocomplete as well as included in the programs help
  text.
-}
openApiLabelArgument :: OpenApi.OpenApiRouter a -> Opt.Parser String
openApiLabelArgument routes =
  let
    labels = OpenApi.openApiLabels routes
  in
    Opt.strArgument
      ( Opt.metavar "LABEL"
          <> Opt.completeWith labels
          <> Opt.help ("labels: " <> List.intercalate "," labels)
      )

parserPrefs :: Opt.ParserPrefs
parserPrefs =
  Opt.prefs $ Opt.showHelpOnEmpty <> Opt.showHelpOnError

{- |
  A main function that prints requested the OpenApi spec from the given
  router to @stdout@ as JSON. If no matching spec is found or if an error
  occurs while generating it, the error will be reported to @stderr@ and
  the process will exit with status code @1@.

  This is intended for users who want to include the functionality of
  the @generate-open-api@ command in their own main functions without
  using @optparse-applicative@.
-}
generateOpenApiMain :: OpenApi.OpenApiRouter a -> String -> IO ()
generateOpenApiMain routes label =
  case OpenApi.mkOpenApi routes label of
    Left err -> do
      IO.hPutStrLn IO.stderr ("Unable to generate OpenApi Spec for " <> label <> "!")
      IO.hPutStrLn IO.stderr err
      Exit.exitWith (Exit.ExitFailure 1)
    Right openApi ->
      LBS.putStr (AesonPretty.encodePretty openApi)
