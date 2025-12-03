module Orb.Main
  ( main
  , mainWithOptions
  , mainParserInfo
  , mainParser
  , mainParserWithCommands
  , openApiOptionsParser
  , openApiLabelArgument
  , generateOpenApiCommand
  , generateOpenApiMain
  ) where

import Data.Aeson.Encode.Pretty qualified as AesonPretty
import Data.ByteString.Lazy qualified as LBS
import Data.Foldable (traverse_)
import Data.List qualified as List
import Data.Set qualified as Set
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
main =
  mainWithOptions OpenApi.defaultOpenApiOptions

{- |
  Constructs a main function that parses the command line arguments and
  provides two subcommands in the executable:

  - @api@ - runs the IO action provided as the main. Presumably this
    invokes some form of 'Orb.Wai.runOrb' (or otherwise runs a way server)
    that serves an application handling the routes provided to this function.

  - @generate-open-api@ - accepts a argument matching one of the labels
    provided to 'OpenApi.provideOpenApi' and prints an OpenApi description
    of the labeled routes as JSON to stdout.


  A version of 'main' takes an 'OpenApi.OpenApiOptions'
  value to use as the default options to the @generate-open-api@
  command.  If you use this function, you probably also want
  to use 'Orb.SwaggerUI.swaggerUIRoutesWithOptions' instead of
  'Orb.SwaggerUI.swaggerUIRoutes' and specify the same options that
  you're passing to this function.
-}
mainWithOptions :: OpenApi.OpenApiOptions -> OpenApi.OpenApiRouter a -> IO () -> IO ()
mainWithOptions defaultOptions routes appMain = do
  io <- Opt.customExecParser parserPrefs (mainParserInfo defaultOptions appMain routes)
  io

{- |
  Constructs a 'Opt.ParserInfo' that will execute as description in 'main', but
  can be used as an argument to 'Opt.command' to use it as a subcommand.

  The options passed will be used as the default values for when no options
  are specified on the command line.
-}
mainParserInfo ::
  OpenApi.OpenApiOptions ->
  IO () ->
  OpenApi.OpenApiRouter a ->
  Opt.ParserInfo (IO ())
mainParserInfo defaultOptions apiMain routes =
  Opt.info (mainParser defaultOptions apiMain routes) mempty

{- |
  Constructs a 'Opt.Parser' that will execute as description in 'main', but
  can be used directly in other option parsers.

  The options passed will be used as the default values for when no options
  are specified on the command line.
-}
mainParser ::
  OpenApi.OpenApiOptions ->
  IO () ->
  OpenApi.OpenApiRouter a ->
  Opt.Parser (IO ())
mainParser defaultOptions apiMain =
  mainParserWithCommands
    defaultOptions
    [ Opt.command "api" (Opt.info (pure apiMain) mempty)
    ]

{- |
  Constructs an 'Opt.Parser' than has the @generate-open-api@ command as in
  the 'main' function along with the other commands passed. In this case no
  @api@ command is added by orb. It is up to the application to passed
  whatever set of other commands it desires.

  The options passed will be used as the default values for when no options
  are specified on the command line.
-}
mainParserWithCommands ::
  OpenApi.OpenApiOptions ->
  [Opt.Mod Opt.CommandFields (IO ())] ->
  OpenApi.OpenApiRouter a ->
  Opt.Parser (IO ())
mainParserWithCommands defaultOptions commands routes =
  Opt.hsubparser $
    mconcat
      ( generateOpenApiCommandWithOptions defaultOptions routes
          : commands
      )

{- |
  Constructs an 'Opt.command' modifier for the @generate-open-api@ command that
  can be used along with 'Opt.hsubparser' include the command wherever the user
  chooses in their options parsing.
-}
generateOpenApiCommand ::
  OpenApi.OpenApiRouter a ->
  Opt.Mod Opt.CommandFields (IO ())
generateOpenApiCommand routes =
  let
    parser =
      generateOpenApiMain
        <$> openApiOptionsParser OpenApi.defaultOpenApiOptions
        <*> pure routes
        <*> openApiLabelArgument routes
  in
    Opt.command "generate-open-api" (Opt.info parser mempty)

{- |
  Constructs an 'Opt.command' modifier for the @generate-open-api@ command that
  can be used along with 'Opt.hsubparser' include the command wherever the user
  chooses in their options parsing.

  The options passed will be used as the default values for when no options
  are specified on the command line.

  A version of 'generateOpenApiCommand' that takes the options argument.
-}
generateOpenApiCommandWithOptions ::
  OpenApi.OpenApiOptions ->
  OpenApi.OpenApiRouter a ->
  Opt.Mod Opt.CommandFields (IO ())
generateOpenApiCommandWithOptions defaultOptions routes =
  let
    parser =
      generateOpenApiMain
        <$> openApiOptionsParser defaultOptions
        <*> pure routes
        <*> openApiLabelArgument routes
  in
    Opt.command "generate-open-api" (Opt.info parser mempty)

{- |
  Constructs a 'Opt.Parser' that will parse command line options to control
  how the OpenApi spec is generated.

  The options passed will be used as the default values for when no options
  are specified on the command line.
-}
openApiOptionsParser :: OpenApi.OpenApiOptions -> Opt.Parser OpenApi.OpenApiOptions
openApiOptionsParser defaultOptions =
  let
    mkOpts allowedChars =
      OpenApi.defaultOpenApiOptions
        { OpenApi.openApiAllowedSchemaNameChars = Set.fromList allowedChars
        }
  in
    mkOpts
      <$> Opt.option
        Opt.str
        ( Opt.long "allowed-chars"
            <> Opt.metavar "CHARS"
            <> Opt.help "e.g. abcdefg12345"
            <> Opt.value (Set.toList (OpenApi.openApiAllowedSchemaNameChars defaultOptions))
            <> Opt.showDefault
        )

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
generateOpenApiMain :: OpenApi.OpenApiOptions -> OpenApi.OpenApiRouter a -> String -> IO ()
generateOpenApiMain options routes label =
  case OpenApi.mkOpenApi options routes label of
    Left errs -> do
      IO.hPutStrLn IO.stderr ("Unable to generate OpenApi Spec for " <> label <> "!")
      traverse_ (IO.hPutStrLn IO.stderr . OpenApi.renderOpenApiError) errs
      Exit.exitWith (Exit.ExitFailure 1)
    Right openApi ->
      LBS.putStr (AesonPretty.encodePretty openApi)
