---
title: Abfragen über die REST-API
published: true
keywords:
    - Vokabularserver
---
Neben Goobi workflow können auch andere Anwendungen die Vokabulare über die REST-API des Vokabularservers lesen, zum Beispiel ein Bestellsystem, das Standorte anzeigen soll. Alle Aufrufe beginnen mit `/api/v1` und benötigen das Token des Vokabularservers im Header `Authorization`.

Die Beispiele auf dieser Seite gehen von einem Vokabularserver unter `http://localhost:8081` mit dem Token `geheimes-token` und vom Vokabular `Standorte` mit der ID `1` aus. Passen Sie Adresse, Token und IDs an Ihre Installation an. Das angehängte `| python3 -m json.tool` formatiert die Antwort nur lesbar und kann entfallen.

## Vokabular und Schema ermitteln
Das Vokabular anhand seines Namens finden. Die Antwort enthält die `id` des Vokabulars und die `schemaId` seines Schemas:

```bash
curl -s -H "Authorization: Bearer geheimes-token" "http://localhost:8081/api/v1/vocabularies/by-name/Standorte" | python3 -m json.tool
```

Alle Vokabulare auflisten:

```bash
curl -s -H "Authorization: Bearer geheimes-token" "http://localhost:8081/api/v1/vocabularies" | python3 -m json.tool
```

Das Schema abrufen. Die `id` jeder Felddefinition wird für die Suche gebraucht:

```bash
curl -s -H "Authorization: Bearer geheimes-token" "http://localhost:8081/api/v1/schemas/1" | python3 -m json.tool
```

## Datensätze abrufen
Die Datensätze eines Vokabulars abrufen:

```bash
curl -s -H "Authorization: Bearer geheimes-token" "http://localhost:8081/api/v1/vocabularies/1/records" | python3 -m json.tool
```

Bei hierarchischen Vokabularen liefert dieser Aufruf nur die Datensätze der obersten Ebene. Untergeordnete Datensätze stehen als IDs im Feld `children`, der übergeordnete Datensatz im Feld `parentId`.

Einen einzelnen Datensatz abrufen:

```bash
curl -s -H "Authorization: Bearer geheimes-token" "http://localhost:8081/api/v1/records/27" | python3 -m json.tool
```

Denselben Datensatz als XML abrufen:

```bash
curl -s -H "Authorization: Bearer geheimes-token" -H "Accept: application/xml" "http://localhost:8081/api/v1/records/27"
```

## Datensätze suchen
Mit dem Parameter `search` werden Datensätze nach dem Inhalt eines Felds gesucht. Die Form ist `Feld-ID:Wert`, wobei die Feld-ID aus dem Schema stammt. Die Suche findet alle Datensätze, deren Feld den Wert enthält, und durchsucht bei hierarchischen Vokabularen den ganzen Baum.

Alle Fächer finden (Feld `Ebene` hat im Beispiel die ID `3`):

```bash
curl -s -H "Authorization: Bearer geheimes-token" "http://localhost:8081/api/v1/vocabularies/1/records?search=3:Fach&size=100" | python3 -m json.tool
```

Alle Datensätze unterhalb von Magazin 2 finden (Feld `Standortcode` hat die ID `1`):

```bash
curl -s -H "Authorization: Bearer geheimes-token" "http://localhost:8081/api/v1/vocabularies/1/records?search=1:HG-M2&size=100" | python3 -m json.tool
```

Leerzeichen im Suchwert werden als `%20` geschrieben:

```bash
curl -s -H "Authorization: Bearer geheimes-token" "http://localhost:8081/api/v1/vocabularies/1/records?search=2:Fach%204" | python3 -m json.tool
```

## Seitenweise Ausgabe
Listen werden seitenweise ausgeliefert, standardmäßig mit 20 Einträgen pro Seite. Mit `size` wird die Seitengröße festgelegt, mit `page` die Seite (beginnend bei `0`). Angaben zur Gesamtzahl stehen im Abschnitt `page` der Antwort.

```bash
curl -s -H "Authorization: Bearer geheimes-token" "http://localhost:8081/api/v1/vocabularies/1/records?search=3:Fach&size=5&page=1" | python3 -m json.tool
```

## Aufbau der Antwort
Die Werte eines Datensatzes stecken in `fields`. Zu welchem Feld des Schemas ein Wert gehört, gibt `definitionId` an. Jeder Wert kann Übersetzungen in mehreren Sprachen haben. Bei nicht übersetzbaren Feldern ist `language` leer. Verkürzt sieht ein Datensatz so aus:

```json
{
    "id": 27,
    "parentId": 23,
    "vocabularyId": 1,
    "fields": [
        {
            "definitionId": 1,
            "values": [ { "translations": [ { "language": null, "value": "HG-M2-R01-RG01-F4" } ] } ]
        },
        {
            "definitionId": 2,
            "values": [ { "translations": [ { "language": null, "value": "Fach 4" } ] } ]
        }
    ]
}
```
