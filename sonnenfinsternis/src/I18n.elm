module I18n exposing (Language(..), Strings, flag, languageName, languages, strings)

{-| All user visible text of the app, in German and Czech.

Every string lives in the `Strings` record, so adding a new one forces both
languages to be filled in – the compiler rejects an incomplete record. Text
that depends on a value takes it as an argument (see `apparentDiameter`,
`typeName`, `typeExplanation`) instead of being assembled by concatenation at
the call site, which keeps word order translatable.

-}

import EclipseType exposing (EclipseType(..))
import Svg as S exposing (Svg)
import Svg.Attributes as SA


type Language
    = German
    | Czech


languages : List Language
languages =
    [ German, Czech ]


{-| Name of the language, in that language – used as the flag's tooltip.
-}
languageName : Language -> String
languageName lang =
    case lang of
        German ->
            "Deutsch"

        Czech ->
            "Čeština"


type alias Strings =
    { title : String
    , intro : String

    -- presets
    , presetsLabel : String

    -- sliders
    , distanceLabel : String
    , distanceNear : String
    , distanceFar : String
    , offsetLabel : String
    , offsetNorth : String
    , offsetSouth : String
    , observerLabel : String
    , observerStart : String
    , observerEnd : String

    -- banner
    , currentType : String

    -- scene
    , sun : String
    , moon : String
    , earth : String
    , observer : String
    , umbraApex : String

    -- legend
    , legendUmbra : String
    , legendPenumbra : String
    , legendAntumbra : String
    , legendPathTotal : String
    , legendPathAnnular : String

    -- observer panel
    , observerPanelTitle : String
    , observerSees : String
    , apparentDiameter : Int -> String
    , seenTotal : ( String, String )
    , seenAnnular : ( String, String )
    , seenPartial : ( String, String )
    , seenNothing : ( String, String )

    -- eclipse types
    , typeName : EclipseType -> String
    , typeShortName : EclipseType -> String
    , typeExplanation : EclipseType -> List String

    -- source note
    , sourcePrefix : String
    , sourceLink : String
    , sourceSuffix : String
    }


strings : Language -> Strings
strings lang =
    case lang of
        German ->
            german

        Czech ->
            czech


german : Strings
german =
    { title = "Arten der Sonnenfinsternis"
    , intro = "Welcher Finsternistyp entsteht, hängt davon ab, wohin Kernschatten und Halbschatten des Mondes fallen. Verändere den Abstand des Mondes und den Versatz seiner Bahn – oder wähle eine Voreinstellung. Die Darstellung ist schematisch und nicht maßstabsgetreu."
    , presetsLabel = "Voreinstellungen:"
    , distanceLabel = "Abstand Erde–Mond"
    , distanceNear = "nah"
    , distanceFar = "fern"
    , offsetLabel = "Bahnversatz des Mondes"
    , offsetNorth = "nördlich"
    , offsetSouth = "südlich"
    , observerLabel = "Beobachter auf dem Finsternispfad"
    , observerStart = "Anfang"
    , observerEnd = "Ende"
    , currentType = "Aktueller Typ: "
    , sun = "Sonne"
    , moon = "Mond"
    , earth = "Erde"
    , observer = "Beobachter"
    , umbraApex = "Spitze des Kernschattens"
    , legendUmbra = "Kernschatten (Umbra)"
    , legendPenumbra = "Halbschatten (Penumbra)"
    , legendAntumbra = "Verlängerter Kernschatten (Antumbra)"
    , legendPathTotal = "Pfad: hier total sichtbar"
    , legendPathAnnular = "Pfad: hier ringförmig sichtbar"
    , observerPanelTitle = "Blick des Beobachters"
    , observerSees = "Der Beobachter sieht: "
    , apparentDiameter =
        \percent ->
            "Scheinbarer Monddurchmesser: " ++ String.fromInt percent ++ " % des Sonnendurchmessers"
    , seenTotal =
        ( "eine totale Verfinsterung"
        , "Die Sonne ist vollständig verdeckt – die Korona leuchtet auf."
        )
    , seenAnnular =
        ( "eine ringförmige Verfinsterung"
        , "Ein heller Sonnenring umgibt die dunkle Mondscheibe."
        )
    , seenPartial =
        ( "eine partielle Verfinsterung"
        , "Der Mond verdeckt die Sonne nur teilweise."
        )
    , seenNothing =
        ( "keine Verfinsterung"
        , "Der Mondschatten trifft diesen Ort nicht."
        )
    , typeName =
        \t ->
            case t of
                Total ->
                    "Totale Sonnenfinsternis"

                Annular ->
                    "Ringförmige Sonnenfinsternis"

                Hybrid ->
                    "Hybride Sonnenfinsternis"

                Partial ->
                    "Partielle Sonnenfinsternis"

                NoEclipse ->
                    "Keine Sonnenfinsternis"
    , typeShortName =
        \t ->
            case t of
                Total ->
                    "Total"

                Annular ->
                    "Ringförmig"

                Hybrid ->
                    "Hybrid"

                Partial ->
                    "Partiell"

                NoEclipse ->
                    "Keine"
    , typeExplanation =
        \t ->
            case t of
                Total ->
                    [ "Der Mond ist der Erde relativ nah: Sein Kernschatten (Umbra) erreicht die Erdoberfläche. Von dort aus erscheint der Mond größer als die Sonne und verdeckt sie vollständig – nur die Sonnenkorona bleibt als leuchtender Kranz sichtbar."
                    , "Wer außerhalb des schmalen Kernschattenpfads im Halbschatten steht, sieht nur eine partielle Finsternis."
                    ]

                Annular ->
                    [ "Der Mond ist relativ weit von der Erde entfernt: Sein Kernschatten endet schon vor der Erdoberfläche. Die Erde liegt im verlängerten Kernschatten (Antumbra)."
                    , "Der Mond erscheint dadurch kleiner als die Sonne und kann sie nicht ganz verdecken – ein heller Sonnenring bleibt sichtbar."
                    ]

                Hybrid ->
                    [ "Der seltene Grenzfall: Die Spitze des Kernschattens reicht gerade eben bis zur Erde. Weil die Erdoberfläche gekrümmt ist, ist die Mitte des Finsternispfads dem Mond um bis zu einen Erdradius näher als Anfang und Ende des Pfads."
                    , "In der Pfadmitte ragt die Erdoberfläche in den Kernschatten hinein – dort ist die Finsternis total. Am Anfang und Ende des Pfads liegt die Oberfläche knapp hinter der Schattenspitze in der Antumbra – dort erscheint sie ringförmig. Ein und dieselbe Finsternis beginnt also ringförmig, wird total und endet wieder ringförmig!"
                    , "Der farbige Rand der Erde zeigt, wo auf dem Pfad die Finsternis total (rot) bzw. ringförmig (orange) erscheint. Verschiebe den Regler „Beobachter auf dem Finsternispfad“, um beide Phasen zu sehen."
                    ]

                Partial ->
                    [ "Kernschatten und Antumbra verfehlen die Erde – nur der Halbschatten (Penumbra) streift sie."
                    , "Der Mond schiebt sich von der Erde aus gesehen nur teilweise vor die Sonne. Nirgendwo auf der Erde ist diese Finsternis total oder ringförmig."
                    ]

                NoEclipse ->
                    [ "Der Schatten des Mondes verfehlt die Erde vollständig – nirgends ist eine Finsternis zu sehen."
                    , "Das ist der Normalfall bei Neumond, weil die Mondbahn um etwa 5° gegen die Erdbahn geneigt ist."
                    ]
    , sourcePrefix = "Begriffe nach dem Wissenskarten-Artikel der "
    , sourceLink = "Medienwerkstatt"
    , sourceSuffix = "."
    }


czech : Strings
czech =
    { title = "Druhy zatmění Slunce"
    , intro = "Jaký typ zatmění nastane, závisí na tom, kam dopadne plný stín a polostín Měsíce. Měň vzdálenost Měsíce a posun jeho dráhy – nebo si vyber přednastavení. Zobrazení je schematické a není v měřítku."
    , presetsLabel = "Přednastavení:"
    , distanceLabel = "Vzdálenost Země–Měsíc"
    , distanceNear = "blízko"
    , distanceFar = "daleko"
    , offsetLabel = "Posun dráhy Měsíce"
    , offsetNorth = "na sever"
    , offsetSouth = "na jih"
    , observerLabel = "Pozorovatel na dráze zatmění"
    , observerStart = "začátek"
    , observerEnd = "konec"
    , currentType = "Aktuální typ: "
    , sun = "Slunce"
    , moon = "Měsíc"
    , earth = "Země"
    , observer = "Pozorovatel"
    , umbraApex = "Vrchol plného stínu"
    , legendUmbra = "Plný stín (umbra)"
    , legendPenumbra = "Polostín (penumbra)"
    , legendAntumbra = "Prodloužený plný stín (antumbra)"
    , legendPathTotal = "Dráha: zde úplné zatmění"
    , legendPathAnnular = "Dráha: zde prstencové zatmění"
    , observerPanelTitle = "Pohled pozorovatele"
    , observerSees = "Pozorovatel vidí: "
    , apparentDiameter =
        \percent ->
            "Zdánlivý průměr Měsíce: " ++ String.fromInt percent ++ " % průměru Slunce"
    , seenTotal =
        ( "úplné zatmění"
        , "Slunce je zcela zakryté – rozzáří se koróna."
        )
    , seenAnnular =
        ( "prstencové zatmění"
        , "Kolem tmavého disku Měsíce svítí jasný sluneční prstenec."
        )
    , seenPartial =
        ( "částečné zatmění"
        , "Měsíc zakrývá Slunce jen zčásti."
        )
    , seenNothing =
        ( "žádné zatmění"
        , "Stín Měsíce toto místo nezasáhne."
        )
    , typeName =
        \t ->
            case t of
                Total ->
                    "Úplné zatmění Slunce"

                Annular ->
                    "Prstencové zatmění Slunce"

                Hybrid ->
                    "Hybridní zatmění Slunce"

                Partial ->
                    "Částečné zatmění Slunce"

                NoEclipse ->
                    "Žádné zatmění Slunce"
    , typeShortName =
        \t ->
            case t of
                Total ->
                    "Úplné"

                Annular ->
                    "Prstencové"

                Hybrid ->
                    "Hybridní"

                Partial ->
                    "Částečné"

                NoEclipse ->
                    "Žádné"
    , typeExplanation =
        \t ->
            case t of
                Total ->
                    [ "Měsíc je Zemi poměrně blízko: jeho plný stín (umbra) dosáhne až na zemský povrch. Odtud se Měsíc jeví větší než Slunce a zakryje ho úplně – viditelný zůstane jen zářivý věnec sluneční koróny."
                    , "Kdo stojí mimo úzký pruh plného stínu, tedy v polostínu, uvidí jen částečné zatmění."
                    ]

                Annular ->
                    [ "Měsíc je od Země poměrně daleko: jeho plný stín končí ještě před zemským povrchem. Země leží v prodlouženém plném stínu (antumbře)."
                    , "Měsíc se proto jeví menší než Slunce a nedokáže ho celé zakrýt – zůstane viditelný jasný sluneční prstenec."
                    ]

                Hybrid ->
                    [ "Vzácný hraniční případ: vrchol plného stínu sahá právě jen tak tak k Zemi. Protože je zemský povrch zakřivený, je střed dráhy zatmění k Měsíci až o jeden zemský poloměr blíž než začátek a konec dráhy."
                    , "Uprostřed dráhy zasahuje zemský povrch do plného stínu – tam je zatmění úplné. Na začátku a na konci dráhy leží povrch těsně za vrcholem stínu v antumbře – tam se jeví jako prstencové. Jedno a totéž zatmění tedy začíná jako prstencové, změní se v úplné a končí opět jako prstencové!"
                    , "Barevný okraj Země ukazuje, kde na dráze je zatmění úplné (červeně) a kde prstencové (oranžově). Posuň táhlem „Pozorovatel na dráze zatmění“ a uvidíš obě fáze."
                    ]

                Partial ->
                    [ "Plný stín i antumbra Zemi minou – zavadí o ni jen polostín (penumbra)."
                    , "Měsíc se ze Země pozorovaný nasune před Slunce jen zčásti. Nikde na Zemi není toto zatmění úplné ani prstencové."
                    ]

                NoEclipse ->
                    [ "Stín Měsíce Zemi zcela mine – nikde není žádné zatmění vidět."
                    , "To je při novu běžný případ, protože dráha Měsíce je vůči dráze Země skloněná asi o 5°."
                    ]
    , sourcePrefix = "Pojmy podle článku Wissenskarten na webu "
    , sourceLink = "Medienwerkstatt"
    , sourceSuffix = "."
    }



-- FLAGS


{-| A 24×16 flag, used as the label of the language switcher button.
-}
flag : Language -> Svg msg
flag lang =
    S.svg
        [ SA.viewBox "0 0 24 16"
        , SA.width "24"
        , SA.height "16"
        , SA.display "block"
        ]
        (case lang of
            German ->
                [ band 0 "#000000"
                , band (16 / 3) "#dd0000"
                , band (32 / 3) "#ffce00"
                , flagBorder
                ]

            Czech ->
                [ S.rect [ SA.x "0", SA.y "0", SA.width "24", SA.height "8", SA.fill "#ffffff" ] []
                , S.rect [ SA.x "0", SA.y "8", SA.width "24", SA.height "8", SA.fill "#d7141a" ] []
                , S.polygon [ SA.points "0,0 12,8 0,16", SA.fill "#11457e" ] []
                , flagBorder
                ]
        )


band : Float -> String -> Svg msg
band y color =
    S.rect
        [ SA.x "0"
        , SA.y (String.fromFloat y)
        , SA.width "24"
        , SA.height (String.fromFloat (16 / 3))
        , SA.fill color
        ]
        []


{-| Keeps the white parts of the Czech flag distinguishable from the page.
-}
flagBorder : Svg msg
flagBorder =
    S.rect
        [ SA.x "0.5"
        , SA.y "0.5"
        , SA.width "23"
        , SA.height "15"
        , SA.fill "none"
        , SA.stroke "#94a3b8"
        , SA.strokeWidth "1"
        ]
        []
