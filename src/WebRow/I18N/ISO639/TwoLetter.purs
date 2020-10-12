module WebRow.I18N.ISO639.TwoLetter where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Symbol (class IsSymbol, SProxy)
import Data.Variant (class Contractable, Variant, contract, inj)
import Data.Variant.Internal (VariantRep(..))
import Foreign.Object (fromHomogeneous, lookup) as Object
import Prim.Row (class Cons) as Row
import Record (get) as Record
import Unsafe.Coerce (unsafeCoerce)
import WebRow.Contrib.Data.Variant (tag) as Variant

newtype LanguageCode = LanguageCode String
derive instance eqLanguageCode ∷ Eq LanguageCode
instance showLanguageCode ∷ Show LanguageCode where
  show (LanguageCode c) = "(LanguageCode " <> show c <> ")"

newtype LanguageNames = LanguageNames
   { english ∷ String
   , nativeName ∷ String
   }
derive instance eqLanguageNames ∷ Eq LanguageNames
derive instance newtypeLanguageNames ∷ Newtype LanguageNames _
instance showLanguageNames ∷ Show LanguageNames where
  show (LanguageNames c) = "(LanguageNames " <> show c <> ")"

type Languages = LanguageBase LanguageNames

code ∷ ∀ r. Contractable Languages r ⇒ Variant r → LanguageCode
code v = LanguageCode (Variant.tag v)

toString ∷ LanguageCode → String
toString (LanguageCode c) = c

get
  ∷ ∀ code langs_ langs__ langs
  . IsSymbol code
  ⇒ Row.Cons code LanguageNames langs_ Languages
  ⇒ Row.Cons code LanguageNames langs__ langs
  ⇒ SProxy code
  → Variant langs
get c =
  let
    lang = Record.get c languages
  in
    inj c lang

parseAny ∷ String → Maybe (Variant Languages)
parseAny =
  let
    all = Object.fromHomogeneous languages
  in \c →
    case Object.lookup c all of
      Just lang → Just $ unsafeCoerce $ VariantRep { type: c, value: lang }
      Nothing → Nothing

parse
  ∷ ∀ langs
  . Contractable Languages langs
  ⇒ String
  → Maybe (Variant langs)
parse = contract <=< parseAny

type LanguageBase a =
  ( aa ∷ a, ab ∷ a, ae ∷ a, af ∷ a, ak ∷ a, am ∷ a, an ∷ a, ar ∷ a
  , as ∷ a, av ∷ a, ay ∷ a, az ∷ a, ba ∷ a, be ∷ a, bg ∷ a, bh ∷ a
  , bi ∷ a, bm ∷ a, bn ∷ a, bo ∷ a, br ∷ a, bs ∷ a, ca ∷ a, ce ∷ a
  , ch ∷ a, co ∷ a, cr ∷ a, cs ∷ a, cu ∷ a, cv ∷ a, cy ∷ a, da ∷ a
  , de ∷ a, dv ∷ a, dz ∷ a, ee ∷ a, el ∷ a, en ∷ a, eo ∷ a, es ∷ a
  , et ∷ a, eu ∷ a, fa ∷ a, ff ∷ a, fi ∷ a, fj ∷ a, fo ∷ a, fr ∷ a
  , fy ∷ a, ga ∷ a, gd ∷ a, gl ∷ a, gn ∷ a, gu ∷ a, gv ∷ a, ha ∷ a
  , he ∷ a, hi ∷ a, ho ∷ a, hr ∷ a, ht ∷ a, hu ∷ a, hy ∷ a, hz ∷ a
  , ia ∷ a, id ∷ a, ie ∷ a, ig ∷ a, ii ∷ a, ik ∷ a, io ∷ a, is ∷ a
  , it ∷ a, iu ∷ a, ja ∷ a, jv ∷ a, ka ∷ a, kg ∷ a, ki ∷ a, kj ∷ a
  , kk ∷ a, kl ∷ a, km ∷ a, kn ∷ a, ko ∷ a, kr ∷ a, ks ∷ a, ku ∷ a
  , kv ∷ a, kw ∷ a, ky ∷ a, la ∷ a, lb ∷ a, lg ∷ a, li ∷ a, ln ∷ a
  , lo ∷ a, lt ∷ a, lu ∷ a, lv ∷ a, mg ∷ a, mh ∷ a, mi ∷ a, mk ∷ a
  , ml ∷ a, mn ∷ a, mr ∷ a, ms ∷ a, mt ∷ a, my ∷ a, na ∷ a, nb ∷ a
  , nd ∷ a, ne ∷ a, ng ∷ a, nl ∷ a, nn ∷ a, no ∷ a, nr ∷ a, nv ∷ a
  , ny ∷ a, oc ∷ a, oj ∷ a, om ∷ a, or ∷ a, os ∷ a, pa ∷ a, pi ∷ a
  , pl ∷ a, ps ∷ a, pt ∷ a, qu ∷ a, rm ∷ a, rn ∷ a, ro ∷ a, ru ∷ a
  , rw ∷ a, sa ∷ a, sc ∷ a, sd ∷ a, se ∷ a, sg ∷ a, si ∷ a, sk ∷ a
  , sl ∷ a, sm ∷ a, sn ∷ a, so ∷ a, sq ∷ a, sr ∷ a, ss ∷ a, st ∷ a
  , su ∷ a, sv ∷ a, sw ∷ a, ta ∷ a, te ∷ a, tg ∷ a, th ∷ a, ti ∷ a
  , tk ∷ a, tl ∷ a, tn ∷ a, to ∷ a, tr ∷ a, ts ∷ a, tt ∷ a, tw ∷ a
  , ty ∷ a, ug ∷ a, uk ∷ a, ur ∷ a, uz ∷ a, ve ∷ a, vi ∷ a, vo ∷ a
  , wa ∷ a, wo ∷ a, xh ∷ a, yi ∷ a, yo ∷ a, za ∷ a, zh ∷ a, zu ∷ a
  )

languages ∷ { | Languages }
languages =
  { aa: LanguageNames
      { english: "Afar"
      , nativeName: "Afaraf"
      }
  , ab: LanguageNames
      { english: "Abkhaz"
      , nativeName: "аҧсуа бызшәа"
      }
  , ae: LanguageNames
      { english: "Avestan"
      , nativeName: "avesta"
      }
  , af: LanguageNames
      { english: "Afrikaans"
      , nativeName: "Afrikaans"
      }
  , ak: LanguageNames
      { english: "Akan"
      , nativeName: "Akan"
      }
  , am: LanguageNames
      { english: "Amharic"
      , nativeName: "አማርኛ"
      }
  , an: LanguageNames
      { english: "Aragonese"
      , nativeName: "aragonés"
      }
  , ar: LanguageNames
      { english: "Arabic"
      , nativeName: "اللغة العربية"
      }
  , as: LanguageNames
      { english: "Assamese"
      , nativeName: "অসমীয়া"
      }
  , av: LanguageNames
      { english: "Avaric"
      , nativeName: "авар мацӀ"
      }
  , ay: LanguageNames
      { english: "Aymara"
      , nativeName: "aymar aru"
      }
  , az: LanguageNames
      { english: "Azerbaijani"
      , nativeName: "azərbaycan dili"
      }
  , ba: LanguageNames
      { english: "Bashkir"
      , nativeName: "башҡорт теле"
      }
  , be: LanguageNames
      { english: "Belarusian"
      , nativeName: "беларуская мова"
      }
  , bg: LanguageNames
      { english: "Bulgarian"
      , nativeName: "български език"
      }
  , bh: LanguageNames
      { english: "Bihari"
      , nativeName: "भोजपुरी"
      }
  , bi: LanguageNames
      { english: "Bislama"
      , nativeName: "Bislama"
      }
  , bm: LanguageNames
      { english: "Bambara"
      , nativeName: "bamanankan"
      }
  , bn: LanguageNames
      { english: "Bengali"
      , nativeName: "বাংলা"
      }
  , bo: LanguageNames
      { english: "Tibetan Standard"
      , nativeName: "བོད་ཡིག"
      }
  , br: LanguageNames
      { english: "Breton"
      , nativeName: "brezhoneg"
      }
  , bs: LanguageNames
      { english: "Bosnian"
      , nativeName: "bosanski jezik"
      }
  , ca: LanguageNames
      { english: "Catalan"
      , nativeName: "Català"
      }
  , ce: LanguageNames
      { english: "Chechen"
      , nativeName: "нохчийн мотт"
      }
  , ch: LanguageNames
      { english: "Chamorro"
      , nativeName: "Chamoru"
      }
  , co: LanguageNames
      { english: "Corsican"
      , nativeName: "corsu"
      }
  , cr: LanguageNames
      { english: "Cree"
      , nativeName: "ᓀᐦᐃᔭᐍᐏᐣ"
      }
  , cs: LanguageNames
      { english: "Czech"
      , nativeName: "čeština"
      }
  , cu: LanguageNames
      { english: "Old Church Slavonic"
      , nativeName: "ѩзыкъ словѣньскъ"
      }
  , cv: LanguageNames
      { english: "Chuvash"
      , nativeName: "чӑваш чӗлхи"
      }
  , cy: LanguageNames
      { english: "Welsh"
      , nativeName: "Cymraeg"
      }
  , da: LanguageNames
      { english: "Danish"
      , nativeName: "dansk"
      }
  , de: LanguageNames
      { english: "German"
      , nativeName: "Deutsch"
      }
  , dv: LanguageNames
      { english: "Divehi"
      , nativeName: "Dhivehi"
      }
  , dz: LanguageNames
      { english: "Dzongkha"
      , nativeName: "རྫོང་ཁ"
      }
  , ee: LanguageNames
      { english: "Ewe"
      , nativeName: "Eʋegbe"
      }
  , el: LanguageNames
      { english: "Greek"
      , nativeName: "Ελληνικά"
      }
  , en: LanguageNames
      { english: "English"
      , nativeName: "English"
      }
  , eo: LanguageNames
      { english: "Esperanto"
      , nativeName: "Esperanto"
      }
  , es: LanguageNames
      { english: "Spanish"
      , nativeName: "Español"
      }
  , et: LanguageNames
      { english: "Estonian"
      , nativeName: "eesti"
      }
  , eu: LanguageNames
      { english: "Basque"
      , nativeName: "euskara"
      }
  , fa: LanguageNames
      { english: "Persian"
      , nativeName: "فارسی"
      }
  , ff: LanguageNames
      { english: "Fula"
      , nativeName: "Fulfulde"
      }
  , fi: LanguageNames
      { english: "Finnish"
      , nativeName: "suomi"
      }
  , fj: LanguageNames
      { english: "Fijian"
      , nativeName: "Vakaviti"
      }
  , fo: LanguageNames
      { english: "Faroese"
      , nativeName: "føroyskt"
      }
  , fr: LanguageNames
      { english: "French"
      , nativeName: "Français"
      }
  , fy: LanguageNames
      { english: "Western Frisian"
      , nativeName: "Frysk"
      }
  , ga: LanguageNames
      { english: "Irish"
      , nativeName: "Gaeilge"
      }
  , gd: LanguageNames
      { english: "Scottish Gaelic"
      , nativeName: "Gàidhlig"
      }
  , gl: LanguageNames
      { english: "Galician"
      , nativeName: "galego"
      }
  , gn: LanguageNames
      { english: "Guaraní"
      , nativeName: "Avañe\"ẽ"
      }
  , gu: LanguageNames
      { english: "Gujarati"
      , nativeName: "ગુજરાતી"
      }
  , gv: LanguageNames
      { english: "Manx"
      , nativeName: "Gaelg"
      }
  , ha: LanguageNames
      { english: "Hausa"
      , nativeName: "هَوُسَ"
      }
  , he: LanguageNames
      { english: "Hebrew"
      , nativeName: "עברית"
      }
  , hi: LanguageNames
      { english: "Hindi"
      , nativeName: "हिन्दी"
      }
  , ho: LanguageNames
      { english: "Hiri Motu"
      , nativeName: "Hiri Motu"
      }
  , hr: LanguageNames
      { english: "Croatian"
      , nativeName: "hrvatski jezik"
      }
  , ht: LanguageNames
      { english: "Haitian"
      , nativeName: "Kreyòl ayisyen"
      }
  , hu: LanguageNames
      { english: "Hungarian"
      , nativeName: "magyar"
      }
  , hy: LanguageNames
      { english: "Armenian"
      , nativeName: "Հայերեն"
      }
  , hz: LanguageNames
      { english: "Herero"
      , nativeName: "Otjiherero"
      }
  , ia: LanguageNames
      { english: "Interlingua"
      , nativeName: "Interlingua"
      }
  , id: LanguageNames
      { english: "Indonesian"
      , nativeName: "Bahasa Indonesia"
      }
  , ie: LanguageNames
      { english: "Interlingue"
      , nativeName: "Interlingue"
      }
  , ig: LanguageNames
      { english: "Igbo"
      , nativeName: "Asụsụ Igbo"
      }
  , ii: LanguageNames
      { english: "Nuosu"
      , nativeName: "ꆈꌠ꒿ Nuosuhxop"
      }
  , ik: LanguageNames
      { english: "Inupiaq"
      , nativeName: "Iñupiaq"
      }
  , io: LanguageNames
      { english: "Ido"
      , nativeName: "Ido"
      }
  , is: LanguageNames
      { english: "Icelandic"
      , nativeName: "Íslenska"
      }
  , it: LanguageNames
      { english: "Italian"
      , nativeName: "Italiano"
      }
  , iu: LanguageNames
      { english: "Inuktitut"
      , nativeName: "ᐃᓄᒃᑎᑐᑦ"
      }
  , ja: LanguageNames
      { english: "Japanese"
      , nativeName: "日本語"
      }
  , jv: LanguageNames
      { english: "Javanese"
      , nativeName: "basa Jawa"
      }
  , ka: LanguageNames
      { english: "Georgian"
      , nativeName: "ქართული"
      }
  , kg: LanguageNames
      { english: "Kongo"
      , nativeName: "Kikongo"
      }
  , ki: LanguageNames
      { english: "Kikuyu"
      , nativeName: "Gĩkũyũ"
      }
  , kj: LanguageNames
      { english: "Kwanyama"
      , nativeName: "Kuanyama"
      }
  , kk: LanguageNames
      { english: "Kazakh"
      , nativeName: "қазақ тілі"
      }
  , kl: LanguageNames
      { english: "Kalaallisut"
      , nativeName: "kalaallisut"
      }
  , km: LanguageNames
      { english: "Khmer"
      , nativeName: "ខេមរភាសា"
      }
  , kn: LanguageNames
      { english: "Kannada"
      , nativeName: "ಕನ್ನಡ"
      }
  , ko: LanguageNames
      { english: "Korean"
      , nativeName: "한국어"
      }
  , kr: LanguageNames
      { english: "Kanuri"
      , nativeName: "Kanuri"
      }
  , ks: LanguageNames
      { english: "Kashmiri"
      , nativeName: "कश्मीरी"
      }
  , ku: LanguageNames
      { english: "Kurdish"
      , nativeName: "Kurdî"
      }
  , kv: LanguageNames
      { english: "Komi"
      , nativeName: "коми кыв"
      }
  , kw: LanguageNames
      { english: "Cornish"
      , nativeName: "Kernewek"
      }
  , ky: LanguageNames
      { english: "Kyrgyz"
      , nativeName: "Кыргызча"
      }
  , la: LanguageNames
      { english: "Latin"
      , nativeName: "latine"
      }
  , lb: LanguageNames
      { english: "Luxembourgish"
      , nativeName: "Lëtzebuergesch"
      }
  , lg: LanguageNames
      { english: "Ganda"
      , nativeName: "Luganda"
      }
  , li: LanguageNames
      { english: "Limburgish"
      , nativeName: "Limburgs"
      }
  , ln: LanguageNames
      { english: "Lingala"
      , nativeName: "Lingála"
      }
  , lo: LanguageNames
      { english: "Lao"
      , nativeName: "ພາສາ"
      }
  , lt: LanguageNames
      { english: "Lithuanian"
      , nativeName: "lietuvių kalba"
      }
  , lu: LanguageNames
      { english: "Luba-Katanga"
      , nativeName: "Tshiluba"
      }
  , lv: LanguageNames
      { english: "Latvian"
      , nativeName: "latviešu valoda"
      }
  , mg: LanguageNames
      { english: "Malagasy"
      , nativeName: "fiteny malagasy"
      }
  , mh: LanguageNames
      { english: "Marshallese"
      , nativeName: "Kajin M̧ajeļ"
      }
  , mi: LanguageNames
      { english: "Māori"
      , nativeName: "te reo Māori"
      }
  , mk: LanguageNames
      { english: "Macedonian"
      , nativeName: "македонски јазик"
      }
  , ml: LanguageNames
      { english: "Malayalam"
      , nativeName: "മലയാളം"
      }
  , mn: LanguageNames
      { english: "Mongolian"
      , nativeName: "Монгол хэл"
      }
  , mr: LanguageNames
      { english: "Marathi"
      , nativeName: "मराठी"
      }
  , ms: LanguageNames
      { english: "Malay"
      , nativeName: "Bahasa Malaysia"
      }
  , mt: LanguageNames
      { english: "Maltese"
      , nativeName: "Malti"
      }
  , my: LanguageNames
      { english: "Burmese"
      , nativeName: "ဗမာစာ"
      }
  , na: LanguageNames
      { english: "Nauru"
      , nativeName: "Ekakairũ Naoero"
      }
  , nb: LanguageNames
      { english: "Norwegian Bokmål"
      , nativeName: "Norsk bokmål"
      }
  , nd: LanguageNames
      { english: "Northern Ndebele"
      , nativeName: "isiNdebele"
      }
  , ne: LanguageNames
      { english: "Nepali"
      , nativeName: "नेपाली"
      }
  , ng: LanguageNames
      { english: "Ndonga"
      , nativeName: "Owambo"
      }
  , nl: LanguageNames
      { english: "Dutch"
      , nativeName: "Nederlands"
      }
  , nn: LanguageNames
      { english: "Norwegian Nynorsk"
      , nativeName: "Norsk nynorsk"
      }
  , no: LanguageNames
      { english: "Norwegian"
      , nativeName: "Norsk"
      }
  , nr: LanguageNames
      { english: "Southern Ndebele"
      , nativeName: "isiNdebele"
      }
  , nv: LanguageNames
      { english: "Navajo"
      , nativeName: "Diné bizaad"
      }
  , ny: LanguageNames
      { english: "Chichewa"
      , nativeName: "chiCheŵa"
      }
  , oc: LanguageNames
      { english: "Occitan"
      , nativeName: "occitan"
      }
  , oj: LanguageNames
      { english: "Ojibwe"
      , nativeName: "ᐊᓂᔑᓈᐯᒧᐎᓐ"
      }
  , om: LanguageNames
      { english: "Oromo"
      , nativeName: "Afaan Oromoo"
      }
  , or: LanguageNames
      { english: "Oriya"
      , nativeName: "ଓଡ଼ିଆ"
      }
  , os: LanguageNames
      { english: "Ossetian"
      , nativeName: "ирон æвзаг"
      }
  , pa: LanguageNames
      { english: "Panjabi"
      , nativeName: "ਪੰਜਾਬੀ"
      }
  , pi: LanguageNames
      { english: "Pāli"
      , nativeName: "पाऴि"
      }
  , pl: LanguageNames
      { english: "Polish"
      , nativeName: "język polski"
      }
  , ps: LanguageNames
      { english: "Pashto"
      , nativeName: "پښتو"
      }
  , pt: LanguageNames
      { english: "Portuguese"
      , nativeName: "Português"
      }
  , qu: LanguageNames
      { english: "Quechua"
      , nativeName: "Runa Simi"
      }
  , rm: LanguageNames
      { english: "Romansh"
      , nativeName: "rumantsch grischun"
      }
  , rn: LanguageNames
      { english: "Kirundi"
      , nativeName: "Ikirundi"
      }
  , ro: LanguageNames
      { english: "Romanian"
      , nativeName: "Română"
      }
  , ru: LanguageNames
      { english: "Russian"
      , nativeName: "Русский"
      }
  , rw: LanguageNames
      { english: "Kinyarwanda"
      , nativeName: "Ikinyarwanda"
      }
  , sa: LanguageNames
      { english: "Sanskrit"
      , nativeName: "संस्कृतम्"
      }
  , sc: LanguageNames
      { english: "Sardinian"
      , nativeName: "sardu"
      }
  , sd: LanguageNames
      { english: "Sindhi"
      , nativeName: "सिन्धी"
      }
  , se: LanguageNames
      { english: "Northern Sami"
      , nativeName: "Davvisámegiella"
      }
  , sg: LanguageNames
      { english: "Sango"
      , nativeName: "yângâ tî sängö"
      }
  , si: LanguageNames
      { english: "Sinhala"
      , nativeName: "සිංහල"
      }
  , sk: LanguageNames
      { english: "Slovak"
      , nativeName: "slovenčina"
      }
  , sl: LanguageNames
      { english: "Slovene"
      , nativeName: "slovenski jezik"
      }
  , sm: LanguageNames
      { english: "Samoan"
      , nativeName: "gagana fa\"a Samoa"
      }
  , sn: LanguageNames
      { english: "Shona"
      , nativeName: "chiShona"
      }
  , so: LanguageNames
      { english: "Somali"
      , nativeName: "Soomaaliga"
      }
  , sq: LanguageNames
      { english: "Albanian"
      , nativeName: "Shqip"
      }
  , sr: LanguageNames
      { english: "Serbian"
      , nativeName: "српски језик"
      }
  , ss: LanguageNames
      { english: "Swati"
      , nativeName: "SiSwati"
      }
  , st: LanguageNames
      { english: "Southern Sotho"
      , nativeName: "Sesotho"
      }
  , su: LanguageNames
      { english: "Sundanese"
      , nativeName: "Basa Sunda"
      }
  , sv: LanguageNames
      { english: "Swedish"
      , nativeName: "svenska"
      }
  , sw: LanguageNames
      { english: "Swahili"
      , nativeName: "Kiswahili"
      }
  , ta: LanguageNames
      { english: "Tamil"
      , nativeName: "தமிழ்"
      }
  , te: LanguageNames
      { english: "Telugu"
      , nativeName: "తెలుగు"
      }
  , tg: LanguageNames
      { english: "Tajik"
      , nativeName: "тоҷикӣ"
      }
  , th: LanguageNames
      { english: "Thai"
      , nativeName: "ไทย"
      }
  , ti: LanguageNames
      { english: "Tigrinya"
      , nativeName: "ትግርኛ"
      }
  , tk: LanguageNames
      { english: "Turkmen"
      , nativeName: "Türkmen"
      }
  , tl: LanguageNames
      { english: "Tagalog"
      , nativeName: "Wikang Tagalog"
      }
  , tn: LanguageNames
      { english: "Tswana"
      , nativeName: "Setswana"
      }
  , to: LanguageNames
      { english: "Tonga"
      , nativeName: "faka Tonga"
      }
  , tr: LanguageNames
      { english: "Turkish"
      , nativeName: "Türkçe"
      }
  , ts: LanguageNames
      { english: "Tsonga"
      , nativeName: "Xitsonga"
      }
  , tt: LanguageNames
      { english: "Tatar"
      , nativeName: "татар теле"
      }
  , tw: LanguageNames
      { english: "Twi"
      , nativeName: "Twi"
      }
  , ty: LanguageNames
      { english: "Tahitian"
      , nativeName: "Reo Tahiti"
      }
  , ug: LanguageNames
      { english: "Uyghur"
      , nativeName: "ئۇيغۇرچە‎"
      }
  , uk: LanguageNames
      { english: "Ukrainian"
      , nativeName: "Українська"
      }
  , ur: LanguageNames
      { english: "Urdu"
      , nativeName: "اردو"
      }
  , uz: LanguageNames
      { english: "Uzbek"
      , nativeName: "Ўзбек"
      }
  , ve: LanguageNames
      { english: "Venda"
      , nativeName: "Tshivenḓa"
      }
  , vi: LanguageNames
      { english: "Vietenglishse"
      , nativeName: "Tiếng Việt"
      }
  , vo: LanguageNames
      { english: "Volapük"
      , nativeName: "Volapük"
      }
  , wa: LanguageNames
      { english: "Walloon"
      , nativeName: "walon"
      }
  , wo: LanguageNames
      { english: "Wolof"
      , nativeName: "Wollof"
      }
  , xh: LanguageNames
      { english: "Xhosa"
      , nativeName: "isiXhosa"
      }
  , yi: LanguageNames
      { english: "Yiddish"
      , nativeName: "ייִדיש"
      }
  , yo: LanguageNames
      { english: "Yoruba"
      , nativeName: "Yorùbá"
      }
  , za: LanguageNames
      { english: "Zhuang"
      , nativeName: "Saɯ cueŋƅ"
      }
  , zh: LanguageNames
      { english: "Chinese"
      , nativeName: "中文"
      }
  , zu: LanguageNames
      { english: "Zulu"
      , nativeName: "isiZulu"
      }
  }


