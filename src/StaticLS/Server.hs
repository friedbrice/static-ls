{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE ScopedTypeVariables #-}

module StaticLS.Server (
    runServer,
) where

--- Standard imports

import Control.Monad.Reader
import Control.Monad.Trans.Except

--- Uncommon 3rd-party imports

import Language.LSP.Server (
    Handlers,
    LanguageContextEnv,
    LspT,
    ServerDefinition (..),
    type (<~>) (Iso),
 )

import Language.LSP.Protocol.Message
import Language.LSP.Protocol.Types
import qualified Language.LSP.Server as LSP

---- Local imports

import StaticLS.IDE.CodeActions
import StaticLS.IDE.Definition
import StaticLS.IDE.Hover
import StaticLS.IDE.References
import StaticLS.IDE.Workspace.Symbol
import StaticLS.StaticEnv
import StaticLS.StaticEnv.Options

-------------------------------------------------------------------------

-----------------------------------------------------------------
--------------------- LSP event handlers ------------------------
-----------------------------------------------------------------

handleChangeConfiguration :: Handlers (LspT c StaticLs)
handleChangeConfiguration = LSP.notificationHandler SMethod_WorkspaceDidChangeConfiguration $ pure $ pure ()

handleInitialized :: Handlers (LspT c StaticLs)
handleInitialized = LSP.notificationHandler SMethod_Initialized $ pure $ pure ()

handleTextDocumentHoverRequest :: Handlers (LspT c StaticLs)
handleTextDocumentHoverRequest = LSP.requestHandler SMethod_TextDocumentHover $ \req resp -> do
    let hoverParams = req._params
    hover <- lift $ retrieveHover hoverParams._textDocument hoverParams._position
    resp $ Right $ maybeToNull hover

handleDefinitionRequest :: Handlers (LspT c StaticLs)
handleDefinitionRequest = LSP.requestHandler SMethod_TextDocumentDefinition $ \req resp -> do
    let defParams = req._params
    defs <- lift $ getDefinition defParams._textDocument defParams._position
    resp $ Right . InR . InL $ defs

handleTypeDefinitionRequest :: Handlers (LspT c StaticLs)
handleTypeDefinitionRequest = LSP.requestHandler SMethod_TextDocumentTypeDefinition $ \req resp -> do
    let typeDefParams = req._params
    defs <- lift $ getTypeDefinition typeDefParams._textDocument typeDefParams._position
    resp $ Right . InR . InL $ defs

handleReferencesRequest :: Handlers (LspT c StaticLs)
handleReferencesRequest = LSP.requestHandler SMethod_TextDocumentReferences $ \req res -> do
    let refParams = req._params
    refs <- lift $ findRefs refParams._textDocument refParams._position
    res $ Right . InL $ refs

handleCancelNotification :: Handlers (LspT c StaticLs)
handleCancelNotification = LSP.notificationHandler SMethod_CancelRequest $ \_ -> pure ()

handleDidOpen :: Handlers (LspT c StaticLs)
handleDidOpen = LSP.notificationHandler SMethod_TextDocumentDidOpen $ \_ -> pure ()

handleDidChange :: Handlers (LspT c StaticLs)
handleDidChange = LSP.notificationHandler SMethod_TextDocumentDidChange $ \_ -> pure ()

handleDidClose :: Handlers (LspT c StaticLs)
handleDidClose = LSP.notificationHandler SMethod_TextDocumentDidClose $ \_ -> pure ()

handleDidSave :: Handlers (LspT c StaticLs)
handleDidSave = LSP.notificationHandler SMethod_TextDocumentDidSave $ \_ -> pure ()

handleWorkspaceSymbol :: Handlers (LspT c StaticLs)
handleWorkspaceSymbol = LSP.requestHandler SMethod_WorkspaceSymbol $ \req res -> do
    -- https://hackage.haskell.org/package/lsp-types-1.6.0.0/docs/Language-LSP-Types.html#t:WorkspaceSymbolParams
    symbols <- lift (symbolInfo req._params._query)
    res $ Right . InL $ symbols

handleSetTrace :: Handlers (LspT c StaticLs)
handleSetTrace = LSP.notificationHandler SMethod_SetTrace $ \_ -> pure ()

handleCodeActionsRequest :: Handlers (LspT c StaticLs)
handleCodeActionsRequest = LSP.requestHandler SMethod_TextDocumentCodeAction $ \req res -> do
    let codeActionParams = req._params
    actions <- lift $ getCodeActions codeActionParams._textDocument codeActionParams._range codeActionParams._context
    res $ Right . InL $ actions

-----------------------------------------------------------------
----------------------- Server definition -----------------------
-----------------------------------------------------------------

data LspEnv config = LspEnv
    { staticEnv :: StaticEnv
    , config :: LanguageContextEnv config
    }

initServer :: StaticEnvOptions -> LanguageContextEnv config -> TMessage 'Method_Initialize -> IO (Either (TResponseError 'Method_Initialize) (LspEnv config))
initServer staticEnvOptions serverConfig _ = do
    runExceptT $ do
        wsRoot <- ExceptT $ LSP.runLspT serverConfig getWsRoot
        serverStaticEnv <- ExceptT $ Right <$> initStaticEnv wsRoot staticEnvOptions
        pure $
            LspEnv
                { staticEnv = serverStaticEnv
                , config = serverConfig
                }
  where
    getWsRoot :: LSP.LspM config (Either (TResponseError 'Method_Initialize) FilePath)
    getWsRoot = do
        mRootPath <- LSP.getRootPath
        pure $ case mRootPath of
            Nothing -> Left $ TResponseError (InR ErrorCodes_InvalidRequest) "No root workspace was found" Nothing
            Just p -> Right p

serverDef :: StaticEnvOptions -> ServerDefinition ()
serverDef argOptions =
    ServerDefinition
        { onConfigChange = \_conf -> pure ()
        , configSection = ""
        , parseConfig = \_conf _value -> Right ()
        , doInitialize = initServer argOptions
        , -- TODO: Do handlers need to inspect clientCapabilities?
          staticHandlers = \_clientCapabilities ->
            mconcat
                [ handleInitialized
                , handleChangeConfiguration
                , handleTextDocumentHoverRequest
                , handleDefinitionRequest
                , handleTypeDefinitionRequest
                , handleReferencesRequest
                , handleCancelNotification
                , handleDidOpen
                , handleDidChange
                , handleDidClose
                , handleDidSave
                , handleWorkspaceSymbol
                , handleSetTrace
                , handleCodeActionsRequest
                ]
        , interpretHandler = \env -> Iso (runStaticLs env.staticEnv . LSP.runLspT env.config) liftIO
        , options =
            LSP.defaultOptions
                { LSP.optTextDocumentSync =
                    Just
                        TextDocumentSyncOptions
                            { _openClose = Just True
                            , _change = Just TextDocumentSyncKind_Full
                            , _willSave = Nothing
                            , _willSaveWaitUntil = Nothing
                            , _save = Nothing
                            }
                }
        , defaultConfig = ()
        }

runServer :: StaticEnvOptions -> IO Int
runServer argOptions = do
    LSP.runServer (serverDef argOptions)
