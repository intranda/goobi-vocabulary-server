---
title: Anleitung zur Erstellung von Vokabularen
published: true
---
Vokabulare sind ein sehr leistungsfähiges Instrument zur Speicherung beliebiger strukturierter Daten.
Daher ist der Prozess der Erstellung von Vokabularen nicht trivial.
In dieser Dokumentation werden wir alle Aspekte von Vokabularen abdecken, die für deren korrekte Verwendung erforderlich sind.
Sie können diese Dokumentation von oben nach unten durcharbeiten.

Jedes der beteiligten Elemente muss durch API-Aufrufe an den Vokabularserver erstellt werden. 
Sie können diese API-Aufrufe zum Beispiel mit `CURL` durchführen. 
Alle Aufrufe folgen weitgehend dem gleichen Muster.
Sie müssen den `BASE_PART` auf `localhost:8081/api/v1` setzen, den Hostnamen und den Port entsprechend ändern und den `/api/v1` unangetastet lassen.
Ändern Sie nur die Teile `API_ENDPOINT` und `DATA`, abhängig von der Art der Abfrage, die Sie durchführen:

```bash
curl -s --location "http://BASE_PART/API_ENDPOINT" --header "Authorization: Bearer TOKEN" --header 'Content-Type: application/json' --data "DATA"
```

Wenn Sie Fehler erhalten, werden die Fehlerinformationen als komplexe JSON-Objekte zurückgegeben.
Sie können das Ergebnis in `jq` einspeisen, um die entsprechende Fehlermeldung wie folgt zu extrahieren:

```bash
curl_call | jq ".message"
```

Wenn der Aufruf erfolgreich ist, erhalten Sie das API-Element als Antwort.
Die meisten Vokabularelemente referenzieren sich gegenseitig über ihre ID.
Um die generierte ID für ein eingefügtes Element zu erhalten, können Sie auch hier `jq` verwenden:

```bash
curl_call | jq ".id"
```

## Überblick
Wir möchten mit einer vereinfachten Übersicht beginnen, um Ihnen den Einstieg zu erleichtern.

*Vokabulare* sind lediglich eine Sammlung von Dateneinträgen, sogenannten *Vokabulardatensätzen*.
Alle Vokabulardatensätze in einem Vokabular entsprechen demselben *Vokabularschema*, das Teil des Vokabulars ist.
Ein Vokabularschema legt die möglichen *Felder* fest, die ein Vokabulardatensatz enthalten kann.
Die Felder eines Vokabeldatensatzes können einen oder mehrere Werte haben, möglicherweise mit mehreren Übersetzungen.
Auf der untersten Ebene stellen diese übersetzbaren Werte Daten bestimmter *Feldtypen* dar.

## Sprachen
Wenn Sie in Ihrem Vokabular mit Sprachen und übersetzbaren Werten arbeiten wollen, müssen Sie diese Sprachen zunächst definieren.
Jede Sprache besteht aus den folgenden Informationen:
- Einem eindeutigen Namen (`name`)
- Eine Sprachabkürzung (`abbreviation`)

Sie können Sprachen mit API-Aufrufen an den  `languages` API Endpunkt erstellen:

```json
{
    "abbreviation": "eng",
    "name": "English"
}
```

## Feldtypen
Am untersten Ende des Vokabulars stehen die Werte.
Jeder Wert gehört zu einem bestimmten Feldtypen.
Diese Typen sind nicht festgelegt und können vom Nutzer bis zu einem gewissen Grad frei definiert werden.
Ein Feldtyp besteht aus den folgenden Informationen:
- Einem eindeutigen Namen (`name`).
- Einem optionalen regulären Ausdruck zur Validierung (`validation`).
- Einem Schalter um anzugeben, dass es sich um "große" Daten handelt (`large`).
- Einer optionalen Menge von eingeschränkten, auswählbaren Werten (`selectableValues`).

Sie können Feldtypen erstellen, indem Sie API-Aufgrufe an den API-Endpunkt `types` mit den folgenden Beispielen senden.

Ein normaler Feldtyp, der jede Art von Daten enthalten kann, könnte wie folgt angegeben werden:

```json
{
    "name": "Irgendetwas"
}
```

Ein numerischer Feldtyp, der Werte von `1` bis `5` enthalten kann (z. B. eine Produktbewertung), kann wie folgt definiert werden:

```json
{
    "name": "Bewertung",
    "selectableValues": [
        "1", "2", "3", "4", "5"
    ]
}
```

Sie können genau den gleichen Feldtypen auch erreichen, indem Sie dies durch eine Validierung mit einem regulären Ausdruck lösen:

```json
{
    "name": "Bewertung",
    "validation": "[1-5]"
}
```

Wenn Sie es versäumt haben, die IDs zu speichern, können Sie den folgenden Befehl aufrufen, um alle vorhandenen Feldtypen mit ihren IDs abzurufen:

```bash
curl -s --location "http://BASE_PART/types" --header "Authorization: Bearer TOKEN" --header 'Content-Type: application/json' | jq '._embedded .fieldTypeList .[] | "\(.id) \(.name)"'
```

*Bitte beachten Sie den fehlenden `--data` Parameter in diesem Aufruf.*

## Vokabularschemata
Ein Vokabularschema definiert die Struktur eines Vokabulars, d. h. alle verfügbaren Felder, die jeder Vokabulardatensatz enthalten kann.
Ein Vokabularschema besteht aus den folgenden Informationen:
- Eine Liste von Felddefinitionen (`definitions`).
- Einem Schalter zur Aktivierung von hierarchischen Datensätzen (`hierarchicalRecords`).
- Einem Schalter zur Beschränkung des Vokabulars auf ein einziges Wurzelelement (`singleRootElement`).

Sie können Vokabularschemata mit Aufrufen an den API-Endpunkt `Schemas` erstellen.

Lassen Sie uns zunächst die letzten beiden Teile besprechen, bevor wir uns mit den Felddefinitionen befassen.
Normalerweise enthalten Vokabulare nur eine potenziell lange Liste von Vokabulardatensätzen.
Es ist jedoch möglich, Vokabulardatensätze hierarchisch zu strukturieren (z. B. ein Datensatz "Material" mit einem untergeordneten Element "Eisen").
Wenn Sie diese Hierarchiefunktion aktivieren wollen, aktivieren Sie diese Option mit:

```json
"hierarchicalRecords": true
```

Wenn Sie Ihr Vokabular so einschränken wollen, dass nur ein einziges Wurzelelement zulässig ist (dies ist vor allem bei hierarchischen Vokabularen nützlich, da Sie sonst nur einen einzigen Eintrag zulassen würden), aktivieren Sie diese Option mit:

```json
"singleRootElement": false
```

Wenn Sie keinen dieser Werte festlegen, werden sie standardmäßig deaktiviert.

Konzentrieren wir uns nun auf die Felddefinitionen.
Jede Felddefinition besteht aus den folgenden Informationen:
- Ein eindeutiger Name unter allen anderen Definitionen (`name`).
- Entweder eine Feldtypen-ID (`typeId`), die auf einen vorhandenen Feldtyp verweist (siehe vorheriger Abschnitt) oder eine Vokabular-ID (`referenceVocabularyId`), die auf ein vorhandenes Vokabular verweist.
- Ein Schalter, um das Feld als Pflichtfeld zu kennzeichnen (`requried`).
- Ein Schalter, der den Feldwert für alle Vokabulardatensätze im Vokabular als eindeutig festlegt (`unique`).
- Ein Schalter, der das Feld als "Haupteintrag" des Vokabulardatensatzes markiert (`mainEntry`).
- Ein Schalter, mit dem das Feld als Titelfeld des Vokabulardatensatzes festgelegt wird (`titleField`).
- Ein Schalter, der das Feld als mehrwertig kennzeichnet (`multiValued`).
- Eine optionale Liste von Übersetzungsdefinitionen (`translationDefinitions`).

Vokabulare stellen strukturierte Daten dar, d. h. alle Datensätze haben die gleichen Felder.
Der Name des Feldes kann alles sein, was die Daten beschreibt, die in diesem Feld gespeichert sind.
Sie können sich den Namen als Spaltennamen vorstellen, wenn Sie Ihre Vokabulardaten tabellarisch visualisieren würden.

Vokabulardatensätze können entweder Werte speichern, die mit einem vorhandenen Feldtyp übereinstimmen, indem ihre `typeId` angegeben wird, oder auf andere Vokabeldatensätze verweisen, indem die entsprechende ID des Vokabulars als `referenceVocabularyId` angegeben wird.
Letzteres könnte nützlich sein, wenn Sie ein Vokabular haben, das Hobbys enthält, zum Beispiel mit den Vokabulardatensätzen "Malen", "Sport", "Reisen".
Stellen Sie sich nun vor, Sie haben ein anderes Vokabular, das berühmte Künstler darstellt, und Sie möchten ein Feld haben, das den Hobbies des Künstlers entspricht.
Anstatt die Hobbies für jeden Künstler zu kopieren und damit Daten zu duplizieren, könnten Sie dieses Feld so einstellen, dass es auf das Vokabular "Hobby" verweist.

Jeder Vokabulardatensatz, der zu diesem Vokabularschema gehört, kann nur Felder definieren, die in den Felddefinitionen (`definitions`) angegeben sind, nicht mehr.
Wenn eine Felddefinition als Pflichtfeld (`required`) festgelegt ist, muss jeder Vokabeldatensatz einen Feldwert enthalten, sonst meldet der Vokabularserver einen Fehler.
Andernfalls ist das Feld optional.

Wenn für eine Felddefinition der Wert als eindeutig (`unique`) festgelegt ist, muss der Wert eines Vokabeldatensatzes in diesem Feld im gesamten Vokabular eindeutig sein.
Das Einfügen oder Ändern eines Vokabeldatensatzes, der einen eindeutigen Wert enthält, der bereits vorhanden ist, führt zu einem Fehler.

Wenn eine Felddefinition das Feld als Haupteintrag festlegt (`mainEntry`), wird der Wert dieses Feldes vorrangig verwendet, wenn irgendwo auf den Vokabulardatensatz verwiesen wird.
Jedes Schema muss genau ein Feld als Hauptfeld definieren.

Ist ein Feld in der Felddefinition als Titelfeld (`titleField`) eingestellt, wird dieses Feld in der tabellarischen Vorschau der Vokabulardatensätze im Goobi-Workflow angezeigt.

Ist eine Felddefinition als mehrwertig (`multiValued`) eingestellt, kann das Feld mehrere Werte enthalten.
Im Falle des zuvor eingeführten Künstler-Vokabulars könnte jeder Künstler mehrere Hobbies haben.
Der Geburtstag eines Künstlers sollte jedoch nicht mehrwertig sein.

Wenn Sie mehrere Sprachen im Vokabular speichern wollen, d. h. Übersetzungen zu den gespeicherten Werten, können Sie Übersetzungsdefinitionen (`translationDefinitions`) definieren.
Wenn Sie dieses Attribut nicht angeben, wird das Feld als nicht übersetzbarer Wert behandelt (z. B. eine Zahl oder ein Datum).
Wenn Sie Übersetzungen anbieten wollen, müssen Sie Übersetzungsdefinitionen der folgenden Form bereitstellen:

```json
{
  "language": "eng",
  "fallback": true,
  "required": true
}
```

Genau eine der Sprachübersetzungen muss als Standardsprache (`fallback`) gesetzt werden.
Die Standardsprache muss immer verpflichtend (`required`) sein.
Jede nicht-verpflichtende Sprache kann leer gelassen werden, wenn später Werte angegeben werden

**Beispiel**
Ein Feld ist so definiert, dass es in Englisch (`eng`) und Deutsch (`ger`) übersetzbar ist, wobei Englisch die verpflichtende Standardsprache und Deutsch die optionale Zweitsprache ist.
Ist in einem Vokabulardatensatz nur die englische Übersetzung angegeben, und dieser Vokabulardatensatz wird später auf Deutsch angefordert, wird in diesem Fall die englische Übersetzung zurückgegeben, weil Englisch die Standardsprache ist.

**Beispiel**
Geben wir ein Beispiel für eine vollständige Vokabularschema-Definition:

```json
{
    "definitions": [
        {
            "name": "Name",
            "typeId": 1,
            "required": true,
            "unique": true,
            "mainEntry": true,
            "titleField": true
        },
        {
            "name": "Beschreibung",
            "typeId": 2,
            "required": false,
            "unique": false,
            "mainEntry": false,
            "titleField": false,
            "translationDefinitions": [
                {
                    "language": "eng",
                    "fallback": true,
                    "required": true
                },
                {
                    "language": "ger",
                    "fallback": false,
                    "required": false
                }
            ]
        },
        {
            "name": "Bewertung",
            "typeId": 3,
            "required": false,
            "unique": false,
            "mainEntry": false,
            "titleField": false
        }
    ]
}
```

Dieses Vokabularschema besteht aus den drei Feldern "Name", "Beschreibung" und "Bewertung".
Es kann verwendet werden, um Filme, Artikel in einem Geschäft oder Lieblingsspielzeug zu beschreiben.
Das Feld "Name" ist als erforderlicher Haupteintrag festgelegt.
Seine Feldtypen-ID `1` sollte dem Feldtypen "Irgendetwas" entsprechen, den wir zuvor erstellt haben.
Das Feld "Beschreibung" sollte eine längere Beschreibung des Eintrags enthalten.
Es hat eine obligatorische englische Übersetzung und eine weitere optionale deutsche Übersetzung.
Seine Feldtypen-ID `2` könnte einem Feldtypen entsprechen, der als "große Daten" eingestellt ist.
Das Feld "Bewertung" sollte einen numerischen Bewertungswert enthalten und dem Feldtypen "Bewertung" entsprechen, den wir zuvor erstellt haben.
Dieses Vokabularschema ist nicht hierarchisch und kann eine beliebige Anzahl von Datensätzen enthalten.

Im [Beispielskript zur Einrichtung des Vokabulars] (../../install/demo.sh) finden Sie einige komplexere Beispiele.
Für diese Beispiele müssen einige Feldtypen vorhanden sein. Sie können diese Standardfeldtypen mit einem anderen [bereitgestellten Skript](../../install/default_setup.sh) installieren.

Nachdem die Vokabularschemata erstellt wurden, können wir nun die Vokabulare erstellen.

## Vokabulare
Um Vokabulare zu erstellen, müssen Sie zunächst deren Vokabularschemata erstellen.
Vokabularschemata können von beliebig vielen Vokabularen wiederverwendet werden (z. B. können Sie das Vokabularschema aus dem vorherigen Abschnitt für die beiden Vokabulare "Filme" und "Amazon Wunschliste" verwenden).

Jedes Vokabular besteht aus den folgenden Informationen:
- Einem eindeutigen Namen (`name`).
- Eine optionale Beschreibung (`description`).
- Eine erforderliche Vokabularschema-ID (`schemaId`).
- Eine optionale Vokabularmetadatenschema-ID (`metadataSchemaId`).

Sie können Vokabularschemata mit Aufrufen an den API-Endpunkt `vocabularies` erstellen.

Der Name muss unter allen Vokabularen eindeutig sein und wird oft verwendet, um ein Vokabular in anderen Goobi Workflow Plugins zu referenzieren.
Die optionale Beschreibung kann ein beliebiger Text sein, der das Vokabular oder seine Daten beschreibt.
Die Vokabularschema-ID muss auf ein bestehendes Vokabularschema verweisen.
Alle Vokabulardatensätze in diesem Vokabular müssen mit dem Vokabularschema mit der ID `schemaID` übereinstimmen.

Wenn Sie zusätzliche Metadaten im Vokabular speichern möchten, die nicht mit einem Vokabulardatensatz verknüpft sind, können Sie mit dem Attribut `metadataSchemaId` ein Metadatenschema angeben.
Sie könnten zum Beispiel ein zusätzliches Schema für die Felder "Erstellungsdatum" und "Autoren" erstellen, um diese zusätzlichen Informationen neben dem Vokabular zu speichern.
Dieses Metadatenschema wird wie jedes andere Schema erstellt.
Um dieses Schema als Metadatenschema in einem Vokabular verwenden zu können, muss das Vokabularschema nicht-hierarchisch und auf ein einziges Wurzelelement beschränkt sein.
Dies ist erforderlich, weil es nur einen Eintrag von Metadaten pro Vokabular geben kann.
Das bedeutet jedoch nicht, dass die Felder des Metadatenschemas nicht mehrwertig sein können!

**Beispiel**
Nehmen wir an, Sie haben ein Metadatenschema mit dem einwertigen Feld "Erstellungsdatum" und dem mehrwertigen Feld "Autoren" erstellt.
Wenn Sie dieses Vokabularschema als Metadatenschema für Vokabulare festlegen, können die Metadaten ein Erstellungsdatum und beliebig viele Autoren enthalten.

**Beispiel**
Lassen Sie uns eine Filmdatenbank mit dem zuvor erstellten Vokabularschema mit der ID `1` erstellen:

```json
{
    "schemaId": 1,
    "name": "MovieDB",
    "description": "Persönliche Filmdatenbank".
}
```

Nachdem Sie alle Vokabulare angelegt haben, können Sie Goobi Workflow mit dem Vokabularserver verbinden und in Goobi selbst Vokabulardatensätze anlegen oder bearbeiten.
Außerdem können Sie Vokabulardatensätze aus verschiedenen Formaten importieren und exportieren.

**RDF**
Der Vokabularverwaltungsserver unterstützt auch spezielle Typen für RDF-konforme Daten.
Damit dies funktioniert, müssen Sie RDF-kompatible Typen erstellen und ein Schema anlegen, das nur aus diesen Typen besteht.
Das resultierende Vokabular kann dann zusätzlich in RDF-Formate wie RDF/XML oder Turtle exportierbar werden.

Wenn Sie das [initiale Feldtypenscript](../../install/default_setup.sh) und das [Demovokabularscript](../../install/demo.sh) verwendet haben, steht Ihnen ein Beispiel für ein solches RDF-Vokabular zur Verfügung.
