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
    validHoleFits,
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
between pfx sfx (Normal text) = do
    (_, _, afterPfx) <- cut pfx text
    (betweenPfxSfx, _, _) <- cut sfx afterPfx
    pure $ Normal betweenPfxSfx
  where
    cut = flip (=~~) :: T.Text -> T.Text -> Maybe (T.Text, T.Text, T.Text)

ident :: T.Text
ident = "[0-9A-Z'\\._a-z]+"

outOfScopeType :: NormalText -> Maybe NormalText
outOfScopeType = capture "Not in scope: type constructor or class ‘" ident "’"

outOfScopeCtor :: NormalText -> Maybe NormalText
outOfScopeCtor = capture "Data constructor not in scope: " ident " :: "

outOfScopeVar :: NormalText -> Maybe NormalText
outOfScopeVar = capture "Variable not in scope: " ident " :: "

perhapsUse :: NormalText -> Maybe NormalText
perhapsUse = capture "Perhaps use ‘" ident "’"

perhapsUseOneOf :: NormalText -> [NormalText]
perhapsUseOneOf = foldMap (captures "‘" ident "’") . between "Perhaps use one of these: " " \\| "

validHoleFits :: NormalText -> [NormalText]
validHoleFits = foldMap (captures " " ident " :: ") . between "Valid hole fits include" " \\| "
