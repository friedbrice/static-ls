-- module StaticLS.IDE.CodeActions (getCodeActions) where
module StaticLS.IDE.CodeActions where

import Control.Monad
import Control.Monad.Reader
import Control.Monad.Trans.Maybe
import Data.Foldable
import Data.Functor
import Data.Maybe
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Text.Encoding (decodeLatin1)
import GHC.Iface.Ext.Types
import Language.Haskell.Syntax (ModuleName, moduleNameString)
import Language.Haskell.TH.LanguageExtensions (Extension)
import Language.LSP.Protocol.Types
import StaticLS.HIE.File (getHieFileFromTdi)
import StaticLS.HieDb
import StaticLS.IDE.CodeActions.Parse
import StaticLS.StaticEnv

newtype Hidden a = Hidden a deriving (Show)
instance Eq (Hidden a) where (==) _ _ = True
instance Ord (Hidden a) where compare _ _ = EQ

getCodeActions :: (MonadReader StaticEnv m, MonadIO m) => TextDocumentIdentifier -> Range -> CodeActionContext -> m [Command |? CodeAction]
getCodeActions tdi rng ctx = do
    let issues = S.fromList (mapMaybe actionableIssue ctx._diagnostics)
    actions <- join <$> traverse (codeActions tdi rng) (S.toList issues)
    pure $ map InR actions

data ActionableIssue
    = RequiredExtension (Hidden Diagnostic) KnownExtension
    | TypedHoleFits (Hidden Diagnostic) [T.Text]
    | OutOfScope (Hidden Diagnostic) T.Text [T.Text]
    deriving (Eq, Ord, Show)

codeActions :: (MonadReader StaticEnv m, MonadIO m) => TextDocumentIdentifier -> Range -> ActionableIssue -> m [CodeAction]
codeActions tdi0 _rng iss = case iss of
    RequiredExtension (Hidden diag) ext -> pure [addRequiredExtension tdi0 diag ext]
    TypedHoleFits (Hidden diag) fits -> pure $ useValidHoleFit tdi0 diag <$> fits
    OutOfScope (Hidden diag) var suggestions -> do
        exporters <- findExporters var
        mHieFile <- runMaybeT $ getHieFileFromTdi tdi0
        let insertRange = fromMaybe (insertAt 0 0) $ findImportInsertRange =<< mHieFile
        let imports = addImport tdi0 diag insertRange var <$> exporters
            similars = useSuggestion tdi0 diag <$> suggestions
        pure $ imports <> similars
  where
    addRequiredExtension :: TextDocumentIdentifier -> Diagnostic -> KnownExtension -> CodeAction
    addRequiredExtension tdi diag (KnownExtension _ text) =
        prefer $ quickFix tdi diag ("Add language extension: " <> text) (insertAt 0 0) (T.concat ["{-# LANGUAGE ", text, " #-}\n"])

    useValidHoleFit :: TextDocumentIdentifier -> Diagnostic -> T.Text -> CodeAction
    useValidHoleFit tdi diag sym = quickFix tdi diag ("Valid hole fit: " <> sym) (diag._range) sym

    findExporters :: (MonadReader StaticEnv m, MonadIO m) => T.Text -> m [ModuleName]
    findExporters var = fmap (fromMaybe []) $ runMaybeT $ runHieDbMaybeT (`searchExports` var)

    findImportInsertRange :: HieFile -> Maybe Range
    findImportInsertRange hieFile =
        fmap (\(n, _) -> insertAt n 0) $
            find (\(_, line) -> T.isPrefixOf "import " line) $
                zip [0 ..] $
                    T.lines $
                        decodeLatin1 hieFile.hie_hs_src

    addImport :: TextDocumentIdentifier -> Diagnostic -> Range -> T.Text -> ModuleName -> CodeAction
    addImport tdi diag insRng var modName =
        let modText = T.pack $ moduleNameString modName
            importLine = T.concat ["import ", modText, " (", var, ")\n"]
         in quickFix tdi diag ("Add import: " <> modText) insRng importLine

    useSuggestion :: TextDocumentIdentifier -> Diagnostic -> T.Text -> CodeAction
    useSuggestion tdi diag suggestion = quickFix tdi diag ("Use: " <> suggestion) diag._range suggestion

actionableIssue :: Diagnostic -> Maybe ActionableIssue
actionableIssue diag =
    asum
        [ checkOutOfScope outOfScopeType
        , checkOutOfScope outOfScopeVar
        , checkOutOfScope outOfScopeCtor
        , checkRequiredExtensions
        , checkValidHoleFits
        ]
  where
    message :: NormalText
    message = normalize diag._message

    suggestions :: [T.Text]
    suggestions = case perhapsUse message of
        Just suggestion -> [getNormalText suggestion]
        Nothing -> map getNormalText $ perhapsUseOneOf message

    checkOutOfScope :: (NormalText -> Maybe NormalText) -> Maybe ActionableIssue
    checkOutOfScope f = do
        var <- f message
        Just $ OutOfScope (Hidden diag) (getNormalText var) suggestions

    checkRequiredExtensions :: Maybe ActionableIssue
    checkRequiredExtensions =
        let checkExt ext@(KnownExtension _ text) =
                guard (T.isInfixOf text $ getNormalText message) $> RequiredExtension (Hidden diag) ext
         in asum $ map checkExt knownExtensions

    checkValidHoleFits :: Maybe ActionableIssue
    checkValidHoleFits = case validHoleFits message of
        [] -> Nothing
        fits -> Just $ TypedHoleFits (Hidden diag) $ map getNormalText fits

insertAt :: UInt -> UInt -> Range
insertAt line col = let p = Position line col in Range p p

prefer :: CodeAction -> CodeAction
prefer action = action{_isPreferred = Just True}

quickFix :: TextDocumentIdentifier -> Diagnostic -> T.Text -> Range -> T.Text -> CodeAction
quickFix tdi diag title range newText =
    CodeAction
        { _title = title
        , _kind = Just CodeActionKind_QuickFix
        , _diagnostics = Just [diag]
        , _edit = Just wsEdit
        , _isPreferred = Nothing
        , _command = Nothing
        , _disabled = Nothing
        , _data_ = Nothing
        }
  where
    wsEdit = WorkspaceEdit Nothing (Just [InL txtDocEdit]) Nothing
    txtDocEdit = TextDocumentEdit txtDoc [InL $ TextEdit range newText]
    txtDoc = OptionalVersionedTextDocumentIdentifier tdi._uri (InR Null)

data KnownExtension = KnownExtension {-# UNPACK #-} !Extension {-# UNPACK #-} !T.Text
    deriving (Eq, Ord, Show)

{-# NOINLINE knownExtensions #-}
knownExtensions :: [KnownExtension]
knownExtensions = (\ext -> KnownExtension ext $ T.pack $ show ext) <$> [minBound .. maxBound]
