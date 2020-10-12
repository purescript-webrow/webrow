module WebRow.I18N.ISO639.TwoLetter where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Symbol (class IsSymbol, SProxy)
import Data.Variant (class Contractable, Variant, contract, inj)
import Data.Variant.Internal (VariantRep(..))
import Foreign.Object (fromHomogeneous, lookup) as Object
import Prim.Row (class Cons) as Row
import Record (get) as Record
import Unsafe.Coerce (unsafeCoerce)
import WebRow.Contrib.Data.Variant (tag) as Variant

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

newtype LanguageCode = LanguageCode String
derive instance eqLanguageCode ∷ Eq LanguageCode
instance showLanguageCode ∷ Show LanguageCode where
  show (LanguageCode c) = "(LanguageCode " <> show c <> ")"

type LanguageNames =
   { english ∷ String
   , nativeName ∷ String
   }

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
  { aa: { english: "Afar"
        , nativeName: "Afaraf"
        }
  , ab: { english: "Abkhaz"
        , nativeName: "аҧсуа бызшәа"
        }
  , ae: { english: "Avestan"
        , nativeName: "avesta"
        }
  , af: { english: "Afrikaans"
        , nativeName: "Afrikaans"
        }
  , ak: { english: "Akan"
        , nativeName: "Akan"
        }
  , am: { english: "Amharic"
        , nativeName: "አማርኛ"
        }
  , an: { english: "Aragonese"
        , nativeName: "aragonés"
        }
  , ar: { english: "Arabic"
        , nativeName: "اللغة العربية"
        }
  , as: { english: "Assamese"
        , nativeName: "অসমীয়া"
        }
  , av: { english: "Avaric"
        , nativeName: "авар мацӀ"
        }
  , ay: { english: "Aymara"
        , nativeName: "aymar aru"
        }
  , az: { english: "Azerbaijani"
        , nativeName: "azərbaycan dili"
        }
  , ba: { english: "Bashkir"
        , nativeName: "башҡорт теле"
        }
  , be: { english: "Belarusian"
        , nativeName: "беларуская мова"
        }
  , bg: { english: "Bulgarian"
        , nativeName: "български език"
        }
  , bh: { english: "Bihari"
        , nativeName: "भोजपुरी"
        }
  , bi: { english: "Bislama"
        , nativeName: "Bislama"
        }
  , bm: { english: "Bambara"
        , nativeName: "bamanankan"
        }
  , bn: { english: "Bengali"
        , nativeName: "বাংলা"
        }
  , bo: { english: "Tibetan Standard"
        , nativeName: "བོད་ཡིག"
        }
  , br: { english: "Breton"
        , nativeName: "brezhoneg"
        }
  , bs: { english: "Bosnian"
        , nativeName: "bosanski jezik"
        }
  , ca: { english: "Catalan"
        , nativeName: "Català"
        }
  , ce: { english: "Chechen"
        , nativeName: "нохчийн мотт"
        }
  , ch: { english: "Chamorro"
        , nativeName: "Chamoru"
        }
  , co: { english: "Corsican"
        , nativeName: "corsu"
        }
  , cr: { english: "Cree"
        , nativeName: "ᓀᐦᐃᔭᐍᐏᐣ"
        }
  , cs: { english: "Czech"
        , nativeName: "čeština"
        }
  , cu: { english: "Old Church Slavonic"
        , nativeName: "ѩзыкъ словѣньскъ"
        }
  , cv: { english: "Chuvash"
        , nativeName: "чӑваш чӗлхи"
        }
  , cy: { english: "Welsh"
        , nativeName: "Cymraeg"
        }
  , da: { english: "Danish"
        , nativeName: "dansk"
        }
  , de: { english: "German"
        , nativeName: "Deutsch"
        }
  , dv: { english: "Divehi"
        , nativeName: "Dhivehi"
        }
  , dz: { english: "Dzongkha"
        , nativeName: "རྫོང་ཁ"
        }
  , ee: { english: "Ewe"
        , nativeName: "Eʋegbe"
        }
  , el: { english: "Greek"
        , nativeName: "Ελληνικά"
        }
  , en: { english: "English"
        , nativeName: "English"
        }
  , eo: { english: "Esperanto"
        , nativeName: "Esperanto"
        }
  , es: { english: "Spanish"
        , nativeName: "Español"
        }
  , et: { english: "Estonian"
        , nativeName: "eesti"
        }
  , eu: { english: "Basque"
        , nativeName: "euskara"
        }
  , fa: { english: "Persian"
        , nativeName: "فارسی"
        }
  , ff: { english: "Fula"
        , nativeName: "Fulfulde"
        }
  , fi: { english: "Finnish"
        , nativeName: "suomi"
        }
  , fj: { english: "Fijian"
        , nativeName: "Vakaviti"
        }
  , fo: { english: "Faroese"
        , nativeName: "føroyskt"
        }
  , fr: { english: "French"
        , nativeName: "Français"
        }
  , fy: { english: "Western Frisian"
        , nativeName: "Frysk"
        }
  , ga: { english: "Irish"
        , nativeName: "Gaeilge"
        }
  , gd: { english: "Scottish Gaelic"
        , nativeName: "Gàidhlig"
        }
  , gl: { english: "Galician"
        , nativeName: "galego"
        }
  , gn: { english: "Guaraní"
        , nativeName: "Avañe\"ẽ"
        }
  , gu: { english: "Gujarati"
        , nativeName: "ગુજરાતી"
        }
  , gv: { english: "Manx"
        , nativeName: "Gaelg"
        }
  , ha: { english: "Hausa"
        , nativeName: "هَوُسَ"
        }
  , he: { english: "Hebrew"
        , nativeName: "עברית"
        }
  , hi: { english: "Hindi"
        , nativeName: "हिन्दी"
        }
  , ho: { english: "Hiri Motu"
        , nativeName: "Hiri Motu"
        }
  , hr: { english: "Croatian"
        , nativeName: "hrvatski jezik"
        }
  , ht: { english: "Haitian"
        , nativeName: "Kreyòl ayisyen"
        }
  , hu: { english: "Hungarian"
        , nativeName: "magyar"
        }
  , hy: { english: "Armenian"
        , nativeName: "Հայերեն"
        }
  , hz: { english: "Herero"
        , nativeName: "Otjiherero"
        }
  , ia: { english: "Interlingua"
        , nativeName: "Interlingua"
        }
  , id: { english: "Indonesian"
        , nativeName: "Bahasa Indonesia"
        }
  , ie: { english: "Interlingue"
        , nativeName: "Interlingue"
        }
  , ig: { english: "Igbo"
        , nativeName: "Asụsụ Igbo"
        }
  , ii: { english: "Nuosu"
        , nativeName: "ꆈꌠ꒿ Nuosuhxop"
        }
  , ik: { english: "Inupiaq"
        , nativeName: "Iñupiaq"
        }
  , io: { english: "Ido"
        , nativeName: "Ido"
        }
  , is: { english: "Icelandic"
        , nativeName: "Íslenska"
        }
  , it: { english: "Italian"
        , nativeName: "Italiano"
        }
  , iu: { english: "Inuktitut"
        , nativeName: "ᐃᓄᒃᑎᑐᑦ"
        }
  , ja: { english: "Japanese"
        , nativeName: "日本語"
        }
  , jv: { english: "Javanese"
        , nativeName: "basa Jawa"
        }
  , ka: { english: "Georgian"
        , nativeName: "ქართული"
        }
  , kg: { english: "Kongo"
        , nativeName: "Kikongo"
        }
  , ki: { english: "Kikuyu"
        , nativeName: "Gĩkũyũ"
        }
  , kj: { english: "Kwanyama"
        , nativeName: "Kuanyama"
        }
  , kk: { english: "Kazakh"
        , nativeName: "қазақ тілі"
        }
  , kl: { english: "Kalaallisut"
        , nativeName: "kalaallisut"
        }
  , km: { english: "Khmer"
        , nativeName: "ខេមរភាសា"
        }
  , kn: { english: "Kannada"
        , nativeName: "ಕನ್ನಡ"
        }
  , ko: { english: "Korean"
        , nativeName: "한국어"
        }
  , kr: { english: "Kanuri"
        , nativeName: "Kanuri"
        }
  , ks: { english: "Kashmiri"
        , nativeName: "कश्मीरी"
        }
  , ku: { english: "Kurdish"
        , nativeName: "Kurdî"
        }
  , kv: { english: "Komi"
        , nativeName: "коми кыв"
        }
  , kw: { english: "Cornish"
        , nativeName: "Kernewek"
        }
  , ky: { english: "Kyrgyz"
        , nativeName: "Кыргызча"
        }
  , la: { english: "Latin"
        , nativeName: "latine"
        }
  , lb: { english: "Luxembourgish"
        , nativeName: "Lëtzebuergesch"
        }
  , lg: { english: "Ganda"
        , nativeName: "Luganda"
        }
  , li: { english: "Limburgish"
        , nativeName: "Limburgs"
        }
  , ln: { english: "Lingala"
        , nativeName: "Lingála"
        }
  , lo: { english: "Lao"
        , nativeName: "ພາສາ"
        }
  , lt: { english: "Lithuanian"
        , nativeName: "lietuvių kalba"
        }
  , lu: { english: "Luba-Katanga"
        , nativeName: "Tshiluba"
        }
  , lv: { english: "Latvian"
        , nativeName: "latviešu valoda"
        }
  , mg: { english: "Malagasy"
        , nativeName: "fiteny malagasy"
        }
  , mh: { english: "Marshallese"
        , nativeName: "Kajin M̧ajeļ"
        }
  , mi: { english: "Māori"
        , nativeName: "te reo Māori"
        }
  , mk: { english: "Macedonian"
        , nativeName: "македонски јазик"
        }
  , ml: { english: "Malayalam"
        , nativeName: "മലയാളം"
        }
  , mn: { english: "Mongolian"
        , nativeName: "Монгол хэл"
        }
  , mr: { english: "Marathi"
        , nativeName: "मराठी"
        }
  , ms: { english: "Malay"
        , nativeName: "Bahasa Malaysia"
        }
  , mt: { english: "Maltese"
        , nativeName: "Malti"
        }
  , my: { english: "Burmese"
        , nativeName: "ဗမာစာ"
        }
  , na: { english: "Nauru"
        , nativeName: "Ekakairũ Naoero"
        }
  , nb: { english: "Norwegian Bokmål"
        , nativeName: "Norsk bokmål"
        }
  , nd: { english: "Northern Ndebele"
        , nativeName: "isiNdebele"
        }
  , ne: { english: "Nepali"
        , nativeName: "नेपाली"
        }
  , ng: { english: "Ndonga"
        , nativeName: "Owambo"
        }
  , nl: { english: "Dutch"
        , nativeName: "Nederlands"
        }
  , nn: { english: "Norwegian Nynorsk"
        , nativeName: "Norsk nynorsk"
        }
  , no: { english: "Norwegian"
        , nativeName: "Norsk"
        }
  , nr: { english: "Southern Ndebele"
        , nativeName: "isiNdebele"
        }
  , nv: { english: "Navajo"
        , nativeName: "Diné bizaad"
        }
  , ny: { english: "Chichewa"
        , nativeName: "chiCheŵa"
        }
  , oc: { english: "Occitan"
        , nativeName: "occitan"
        }
  , oj: { english: "Ojibwe"
        , nativeName: "ᐊᓂᔑᓈᐯᒧᐎᓐ"
        }
  , om: { english: "Oromo"
        , nativeName: "Afaan Oromoo"
        }
  , or: { english: "Oriya"
        , nativeName: "ଓଡ଼ିଆ"
        }
  , os: { english: "Ossetian"
        , nativeName: "ирон æвзаг"
        }
  , pa: { english: "Panjabi"
        , nativeName: "ਪੰਜਾਬੀ"
        }
  , pi: { english: "Pāli"
        , nativeName: "पाऴि"
        }
  , pl: { english: "Polish"
        , nativeName: "język polski"
        }
  , ps: { english: "Pashto"
        , nativeName: "پښتو"
        }
  , pt: { english: "Portuguese"
        , nativeName: "Português"
        }
  , qu: { english: "Quechua"
        , nativeName: "Runa Simi"
        }
  , rm: { english: "Romansh"
        , nativeName: "rumantsch grischun"
        }
  , rn: { english: "Kirundi"
        , nativeName: "Ikirundi"
        }
  , ro: { english: "Romanian"
        , nativeName: "Română"
        }
  , ru: { english: "Russian"
        , nativeName: "Русский"
        }
  , rw: { english: "Kinyarwanda"
        , nativeName: "Ikinyarwanda"
        }
  , sa: { english: "Sanskrit"
        , nativeName: "संस्कृतम्"
        }
  , sc: { english: "Sardinian"
        , nativeName: "sardu"
        }
  , sd: { english: "Sindhi"
        , nativeName: "सिन्धी"
        }
  , se: { english: "Northern Sami"
        , nativeName: "Davvisámegiella"
        }
  , sg: { english: "Sango"
        , nativeName: "yângâ tî sängö"
        }
  , si: { english: "Sinhala"
        , nativeName: "සිංහල"
        }
  , sk: { english: "Slovak"
        , nativeName: "slovenčina"
        }
  , sl: { english: "Slovene"
        , nativeName: "slovenski jezik"
        }
  , sm: { english: "Samoan"
        , nativeName: "gagana fa\"a Samoa"
        }
  , sn: { english: "Shona"
        , nativeName: "chiShona"
        }
  , so: { english: "Somali"
        , nativeName: "Soomaaliga"
        }
  , sq: { english: "Albanian"
        , nativeName: "Shqip"
        }
  , sr: { english: "Serbian"
        , nativeName: "српски језик"
        }
  , ss: { english: "Swati"
        , nativeName: "SiSwati"
        }
  , st: { english: "Southern Sotho"
        , nativeName: "Sesotho"
        }
  , su: { english: "Sundanese"
        , nativeName: "Basa Sunda"
        }
  , sv: { english: "Swedish"
        , nativeName: "svenska"
        }
  , sw: { english: "Swahili"
        , nativeName: "Kiswahili"
        }
  , ta: { english: "Tamil"
        , nativeName: "தமிழ்"
        }
  , te: { english: "Telugu"
        , nativeName: "తెలుగు"
        }
  , tg: { english: "Tajik"
        , nativeName: "тоҷикӣ"
        }
  , th: { english: "Thai"
        , nativeName: "ไทย"
        }
  , ti: { english: "Tigrinya"
        , nativeName: "ትግርኛ"
        }
  , tk: { english: "Turkmen"
        , nativeName: "Türkmen"
        }
  , tl: { english: "Tagalog"
        , nativeName: "Wikang Tagalog"
        }
  , tn: { english: "Tswana"
        , nativeName: "Setswana"
        }
  , to: { english: "Tonga"
        , nativeName: "faka Tonga"
        }
  , tr: { english: "Turkish"
        , nativeName: "Türkçe"
        }
  , ts: { english: "Tsonga"
        , nativeName: "Xitsonga"
        }
  , tt: { english: "Tatar"
        , nativeName: "татар теле"
        }
  , tw: { english: "Twi"
        , nativeName: "Twi"
        }
  , ty: { english: "Tahitian"
        , nativeName: "Reo Tahiti"
        }
  , ug: { english: "Uyghur"
        , nativeName: "ئۇيغۇرچە‎"
        }
  , uk: { english: "Ukrainian"
        , nativeName: "Українська"
        }
  , ur: { english: "Urdu"
        , nativeName: "اردو"
        }
  , uz: { english: "Uzbek"
        , nativeName: "Ўзбек"
        }
  , ve: { english: "Venda"
        , nativeName: "Tshivenḓa"
        }
  , vi: { english: "Vietenglishse"
        , nativeName: "Tiếng Việt"
        }
  , vo: { english: "Volapük"
        , nativeName: "Volapük"
        }
  , wa: { english: "Walloon"
        , nativeName: "walon"
        }
  , wo: { english: "Wolof"
        , nativeName: "Wollof"
        }
  , xh: { english: "Xhosa"
        , nativeName: "isiXhosa"
        }
  , yi: { english: "Yiddish"
        , nativeName: "ייִדיש"
        }
  , yo: { english: "Yoruba"
        , nativeName: "Yorùbá"
        }
  , za: { english: "Zhuang"
        , nativeName: "Saɯ cueŋƅ"
        }
  , zh: { english: "Chinese"
        , nativeName: "中文"
        }
  , zu: { english: "Zulu"
        , nativeName: "isiZulu"
        }
  }


