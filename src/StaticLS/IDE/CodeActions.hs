module StaticLS.IDE.CodeActions (getCodeActions) where

import Control.Applicative
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
getCodeActions tdi _rng ctx = do
    let issues = S.fromList (mapMaybe actionableIssue ctx._diagnostics)
    actions <- join <$> traverse (codeActions tdi) (S.toList issues)
    pure $ map InR actions

data ActionableIssue
    = RequiredExtension (Hidden Diagnostic) KnownExtension
    | TypedHoleFits (Hidden Diagnostic) [T.Text]
    | OutOfScope (Hidden Diagnostic) T.Text [T.Text]
    | MissingFields (Hidden Diagnostic) T.Text (Maybe T.Text) [T.Text]
    | MissingMethods (Hidden Diagnostic) [T.Text]
    | MissingAssociatedType (Hidden Diagnostic) T.Text
    | MissingCasses (Hidden Diagnostic) [T.Text]
    deriving (Eq, Ord, Show)

codeActions :: (MonadReader StaticEnv m, MonadIO m) => TextDocumentIdentifier -> ActionableIssue -> m [CodeAction]
codeActions tdi0 iss = case iss of
    MissingMethods (Hidden diag) methods -> pure [insertMissingMethods tdi0 diag methods]
    MissingAssociatedType (Hidden diag) ty -> pure [insertAssociatedType tdi0 diag ty]
    MissingFields (Hidden diag) ctor ext flds -> pure [insertFields tdi0 diag ctor ext flds]
    MissingCasses (Hidden diag) pats -> pure [insertCases tdi0 diag pats]
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
    insertAssociatedType :: TextDocumentIdentifier -> Diagnostic -> T.Text -> CodeAction
    insertAssociatedType tdi diag ty =
        let rng = insertBelow diag._range
            txt = T.concat ["    type ", ty, " = ()\n"]
         in prefer $ quickFix tdi diag "Insert associated type." rng txt

    insertMissingMethods :: TextDocumentIdentifier -> Diagnostic -> [T.Text] -> CodeAction
    insertMissingMethods tdi diag methods =
        let rng = insertBelow diag._range
            txt = T.concat ["    ", T.intercalate " = _\n    " methods, "\n"]
         in prefer $ quickFix tdi diag "Insert missing methods." rng txt

    insertFields :: TextDocumentIdentifier -> Diagnostic -> T.Text -> Maybe T.Text -> [T.Text] -> CodeAction
    insertFields tdi diag ctor existingFields missingFields =
        let spaces = indentation diag._range
            seps = "{ " : repeat ", "
            formatNewField sep fld = T.concat [spaces, sep, fld, " = _"]
            formatOldFields flds = T.concat [spaces, ", ", flds]
            newFields = zipWith formatNewField seps missingFields
            allFields = case existingFields of
                Nothing -> newFields
                Just flds -> newFields <> [formatOldFields flds]
            renderedExpr = T.concat [ctor, "\n" <> T.unlines allFields <> spaces <> "}\n"]
         in prefer $ quickFix tdi diag "Insert fields." (diag._range) renderedExpr

    insertCases :: TextDocumentIdentifier -> Diagnostic -> [T.Text] -> CodeAction
    insertCases tdi diag pats =
        let spaces = indentation diag._range
            rng = insertBelow diag._range
            cases = foldMap (\pat -> spaces <> pat <> " -> _") pats
         in prefer $ quickFix tdi diag "Insert cases." rng cases

    addRequiredExtension :: TextDocumentIdentifier -> Diagnostic -> KnownExtension -> CodeAction
    addRequiredExtension tdi diag (KnownExtension _ text) =
        let title = "Add language extension: " <> text
            txt = T.concat ["{-# LANGUAGE ", text, " #-}\n"]
            rng = insertAt 0 0
         in prefer $ quickFix tdi diag title rng txt

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
        , checkMissingFields
        , checkMissingCases
        , checkMissingMethods
        , checkMissingAssociatedType
        ]
  where
    message :: NormalText
    message = normalize diag._message

    suggestions :: [T.Text]
    suggestions = do
        sug <- toList (perhapsUse message)
        sugs <- toList (perhapsUseOneOf message)
        getNormalText sug : fmap getNormalText sugs

    checkOutOfScope :: (NormalText -> Maybe NormalText) -> Maybe ActionableIssue
    checkOutOfScope f = do
        var <- f message
        Just $ OutOfScope (Hidden diag) (getNormalText var) suggestions

    checkRequiredExtensions :: Maybe ActionableIssue
    checkRequiredExtensions =
        let checkExt ext@(KnownExtension _ text) =
                guard (T.isInfixOf text $ getNormalText message) $> RequiredExtension (Hidden diag) ext
         in asum $ map checkExt knownExtensions

    checkMissingFields :: Maybe ActionableIssue
    checkMissingFields = do
        (ctor, ext, flds) <- requiredStrictFields message <|> fieldsNotInitialized message
        Just $ MissingFields (Hidden diag) (getNormalText ctor) (fmap getNormalText ext) (map getNormalText flds)

    checkMissingCases :: Maybe ActionableIssue
    checkMissingCases = MissingCasses (Hidden diag) . map getNormalText <$> nonExhaustivePatterns message

    checkValidHoleFits :: Maybe ActionableIssue
    checkValidHoleFits = TypedHoleFits (Hidden diag) . map getNormalText <$> validHoleFits message

    checkMissingMethods :: Maybe ActionableIssue
    checkMissingMethods = MissingMethods (Hidden diag) . map getNormalText <$> missingMethods message

    checkMissingAssociatedType :: Maybe ActionableIssue
    checkMissingAssociatedType = MissingAssociatedType (Hidden diag) . getNormalText <$> missingAssociatedType message

insertAt :: UInt -> UInt -> Range
insertAt line col = let p = Position line col in Range p p

insertBelow :: Range -> Range
insertBelow (Range start _) = insertAt (start._line + 1) 0

indentation :: Range -> T.Text
indentation (Range start _) = T.replicate (fromIntegral start._character + 4) " "

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
