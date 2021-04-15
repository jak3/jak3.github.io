module Multilang where

import qualified Data.Map as M

type Key = String
type Translation = String
type Translations = (M.Map Language Translation)

data Language = English | Italian
  deriving (Ord, Eq, Enum)

instance Show Language where
  show English = "en"
  show Italian = "it"

langs = enumFrom English

dbTranslations :: (M.Map Key Translations)
dbTranslations = M.fromList
  [ ("language"     , M.fromList [(English, "en"          ), (Italian, "it"        )])
  , ("contacts"     , M.fromList [(English, "Contacts"    ), (Italian, "Contatti"  )])
  , ("archive"      , M.fromList [(English, "Archive"     ), (Italian, "Archivio"  )])
  , ("art"          , M.fromList [(English, "Art"         ), (Italian, "Arte"      )])
  , ("review"       , M.fromList [(English, "Review"      ), (Italian, "Recensioni")])
  , ("excerpts"     , M.fromList [(English, "Excerpts"    ), (Italian, "Estratti"  )])
  , ("translations" , M.fromList [(English, "Translations"), (Italian, "Traduzioni")])
  , ("made"         , M.fromList [(English, "Made by"     ), (Italian, "Creato da" )])
  , ("with"         , M.fromList [(English, "with"        ), (Italian, "con"       )])
  , ("en"           , M.fromList [(English, "English"     ), (Italian, "Inglese"   )])
  , ("it"           , M.fromList [(English, "Italian"     ), (Italian, "Italiano"  )])
  , ("notaudio"     , M.fromList [

      (English, "Preview of audio file not supported by your Browser" ),
      (Italian, "Anteprima del file audio non supportata dal Browser" )

     ]
    )
  ]

getTranslation :: Language -> Key -> Maybe Translation
getTranslation l k = M.lookup l $ case M.lookup k dbTranslations of
                                    (Just t) -> t
                                    Nothing  -> M.empty

wordToEnglish :: Key -> Maybe Translation
wordToEnglish = getTranslation English

wordToItalian :: Key -> Maybe Translation
wordToItalian = getTranslation Italian
