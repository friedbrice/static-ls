module StaticLS.IDE.CodeActions.Parse (
    -- * Text wherein all whitespace intervals have been collapsed to one space.
    NormalText,
    normalize,
    getNormalText,

    -- * Parsing GHC errors
    outOfScopeType,
    outOfScopeCtor,
    outOfScopeVar,
    perhapsUse,
    perhapsUseOneOf,
    fieldsNotInitialized,
    requiredStrictFields,
    validHoleFits,
    missingMethods,
    missingAssociatedType,
    nonExhaustivePatterns,
)
where

import Control.Monad
import Data.Foldable
import qualified Data.Text as T
import Text.Regex.TDFA (getAllTextMatches, (=~), (=~~))

newtype NormalText = Normal T.Text
    deriving (Eq, Ord, Show)

getNormalText :: NormalText -> T.Text
getNormalText (Normal txt) = txt

normalize :: T.Text -> NormalText
normalize = Normal . T.unwords . T.words

capture :: T.Text -> T.Text -> T.Text -> NormalText -> Maybe NormalText
capture pfx pat sfx = fmap Normal . (=~~ pat) . getNormalText <=< between pfx sfx

captures :: T.Text -> T.Text -> T.Text -> NormalText -> [NormalText]
captures pfx pat sfx (Normal text) = do
    pfxCapSfx <- getAllTextMatches (text =~ T.concat [pfx, pat, sfx])
    cap <- toList (T.stripPrefix pfx =<< T.stripSuffix sfx pfxCapSfx)
    [Normal cap]

between :: T.Text -> T.Text -> NormalText -> Maybe NormalText
between pfx sfx text = do
    (_, _, afterPfx) <- cut pfx text
    (betweenPfxSfx, _, _) <- cut sfx afterPfx
    pure betweenPfxSfx

cut :: T.Text -> NormalText -> Maybe (NormalText, NormalText, NormalText)
cut pat (Normal txt) = (\(x, y, z) -> (Normal x, Normal y, Normal z)) <$> txt =~~ pat

ident :: T.Text
ident = "[0-9A-Z'\\._a-z]+"

outOfScopeType :: NormalText -> Maybe NormalText
outOfScopeType = capture "Not in scope: type constructor or class ‘" ident "’"

outOfScopeCtor :: NormalText -> Maybe NormalText
outOfScopeCtor = capture "Data constructor not in scope: " ident " ::"

outOfScopeVar :: NormalText -> Maybe NormalText
outOfScopeVar = capture "Variable not in scope: " ident " ::"

perhapsUse :: NormalText -> Maybe NormalText
perhapsUse = capture "Perhaps use ‘" ident "’"

perhapsUseOneOf :: NormalText -> Maybe [NormalText]
perhapsUseOneOf = fmap (captures "‘" ident "’") . between "Perhaps use one of these:" "\\| "

validHoleFits :: NormalText -> Maybe [NormalText]
validHoleFits = fmap (captures " " ident " ::\\>") . between "Valid hole fits include" "\\| "

missingMethods :: NormalText -> Maybe [NormalText]
missingMethods = fmap (captures "‘" ident "’") . between "No explicit implementation for " " • In the instance declaration"

missingAssociatedType :: NormalText -> Maybe NormalText
missingAssociatedType = capture "No explicit associated type or default declaration for ‘" ident "’"

nonExhaustivePatterns :: NormalText -> Maybe [NormalText]
nonExhaustivePatterns = fmap (captures " " (ident <> "( _)*") " ") . between "not matched:" " \\| "

fieldsNotInitialized :: NormalText -> Maybe (NormalText, Maybe NormalText, [NormalText])
fieldsNotInitialized t1 = do
    (_, _, t2) <- cut "Fields of ‘" t1
    (constructor, _, t3) <- cut "’ not initialised: " t2
    (fieldsSection, _, t4) <- cut " • In the expression: " t3
    let missingFields = captures " " ident " :: " fieldsSection
    braces <- between "\\{" "\\}" t4
    let existingFields = if T.null (getNormalText braces) then Nothing else Just braces
    pure ( constructor, existingFields, missingFields)

requiredStrictFields :: NormalText -> Maybe (NormalText, Maybe NormalText, [NormalText])
requiredStrictFields t1 = do
    (_, _, t2) <- cut "Constructor ‘" t1
    (constructor, _, t3) <- cut "’ does not have the required strict field\\(s\\):" t2
    (fieldsSection, _, t4) <- cut "• In the expression:" t3
    let missingFields = captures " " ident " ::" fieldsSection
    braces <- between "\\{" "\\}" t4
    let existingFields = if T.null (getNormalText braces) then Nothing else Just braces
    pure ( constructor, existingFields, missingFields)
