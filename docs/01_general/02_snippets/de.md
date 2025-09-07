---
title: Snippets
published: true
keywords:
    - Vokabularserver
---
Im Folgenden sind einige Snippets aufgeführt, die die Inbetriebnahme des Vokabularservers konkret am Beispiel eine Goobi-to-go-Instanz aufzeigen:

## Test auf Inhalte im Vokabularserver

```bash
curl "http://localhost:8081/api/v1/types" --header "Authorization: Bearer secret" | jq
curl "http://localhost:8081/api/v1/schemas" --header "Authorization: Bearer secret" | jq
curl "http://localhost:8081/api/v1/vocabularies" --header "Authorization: Bearer secret" | jq
```

## Herunterladen des Installationsskripts

```bash
wget https://github.com/intranda/goobi-vocabulary-server/releases/latest/download/vocabulary-init-script.zip
unzip vocabulary-init-script.zip
```

## Beispiel für das Anlegen minimaler Typen

```bash
HOST=localhost PORT=8081 TOKEN=secret vocabulary-init-script/install.sh minimal
```

## Beispiel für das Anlegen von Vokabularen für Bücher

```bash
HOST=localhost PORT=8081 TOKEN=secret vocabulary-init-script/install.sh books
```


## Beispiel für das Anlegen von Vokabularen für Sammlungen

```bash
HOST=localhost PORT=8081 TOKEN=secret vocabulary-init-script/install.sh collections
```