# Einrichtung
Diese Dokumentation beschreibt den Prozess der Installation und Ersteinrichtung des Vokabularservers.

## Download und Installation
- Laden Sie die [Neuste Version](https://jenkins.intranda.com/job/intranda/job/vocabulary-server/job/develop/lastSuccessfulBuild/artifact/module-core/target/) des Vokabularservers herunter.
- Laden Sie die [Konfigurationsdatei](https://jenkins.intranda.com/job/intranda/job/vocabulary-server/job/develop/lastSuccessfulBuild/artifact/module-core/src/main/resources/application.properties) des Vokabularservers herunter.
- Passen Sie die Konfigurationsdatei entsprechend Ihrer Konfiguration an und entfernen Sie nicht geänderte Zeilen.
    - Datenbankanmeldeinformationen und Datenbankname.
    - Basis-URL und Port.
- **TODO** *Installieren Sie die `vocabulary-server.jar` und die Konfigurationsdatei `application.properties` direkt in einen neuen Ordner (z. B. `/opt/digiverso/vocabulary/`)*

## Als systemd-Dienst starten
- **TODO** *Erstellen einer systemd Service Unit für den Vokabularserver (Die Anwendung sollte bei SIGTERM korrekt herunterfahren können)*
- **TODO** *Admin-Dokumentation hier*
- Führen Sie `java -jar vocabulary-server-VERSION.jar` aus.
- Wenn der Start erfolgreich war, werden Sie nach ein paar Sekunden eine Zeile wie diese sehen:
```bash
Started VocabularyServerApplication in 4.244 seconds (process running for 4.581)
```

## Einrichtung von Goobi Workflow zur Kommunikation mit dem Vokabularserver
- Goobi Workflow verwendet seit Version `24.07` den neuen Vokabularserver.
- Konfigurieren Sie die Variablen `vocabularyServerHost` und `vocabularyServerPort` in der Datei `goobi_config.properties` entsprechend der Konfiguration Ihres Vokabularservers.
- Starten Sie Goobi Workflow neu, damit die Änderungen wirksam werden.
- Navigieren Sie zu `Administration` > `Vocabulare`, um zu überprüfen, ob alles funktioniert. Sie sollten eine Liste von Vokabularen sehen, wenn alles in Ordnung ist (nicht jetzt, sondern nachdem Sie einige Vokabulare erstellt oder die bestehenden migriert haben). Wenn etwas nicht funktioniert, wird eine rote Fehlermeldung angezeigt.

## Ersteinrichtung
- Für den ordnungsgemäßen Betrieb benötigt der Vokabularserver einige Ausgangsdaten.
- Diese Daten enthalten Sprachangaben (wenn mehrsprachige Vokabulare verwendet werden) und Feldtypdefinitionen. 
- Sie können das folgende Skript verwenden, welches einige Beispielsprachen und Feldtypen installiert.
- Laden Sie das [Initial Data Script](https://jenkins.intranda.com/job/intranda/job/vocabulary-server/job/develop/lastSuccessfulBuild/artifact/install/default_setup.sh) herunter.
- Ändern Sie die Variable `HOST` am Anfang entsprechend der Konfiguration des Vokabularservers, lassen Sie das Suffix `/api/v1` unverändert.
- Führen Sie das Skript aus.

## Sicherheit
- Sie können Apache-URL-Beschränkungen einrichten, um den Vokabularserver vor unberechtigtem Zugriff zu schützen.
- **TODO** *Admins, bitte finden Sie heraus, was und wie man es im Detail macht.*

## Installationstest
- Ändern Sie für alle Befehle Host und Port entsprechend.
- Prüfen Sie nach der Ersteinrichtung, ob die Feldtypen erfolgreich erstellt wurden:
```bash
curl http://localhost:8081/api/v1/types | jq -r '._embedded.fieldTypeList[] .name'
```
- Das Ergebnis sollte wie folgt aussehen:
```bash
Anything
Boolean
Number
Word
skos:prefLabel
skos:altLabel
skos:definition
skos:editorialNote
skos:related
skos:closeMatch
skos:exactMatch
```
- Wenn eine Datenmigration stattgefunden hat, prüfen Sie, ob alle Vokabulare migriert wurden:
```bash
curl http://localhost:8081/api/v1/vocabularies/all | jq -r '._embedded.vocabularyList[] .name'                
```
- Prüfen Sie, ob die Links korrekt aufgelöst werden (siehe Konfiguration):
```bash
curl http://localhost:8081/api/v1/records/1 | jq
```
Das JSON-Element `_links` sollte Verweise auf andere Ressourcen enthalten. 
Diese URLs sollten gültig und auflösbar sein. 
Wenn Sie keinen dieser Verweise öffnen können, überprüfen Sie die Konfiguration des Vokabularservers (Konfigurationsoption `vocabulary-server.base-url`).
Bei Problemen mit diesen URLs müssen die Daten nicht neu importiert werden. 
Aktualisieren Sie einfach die Konfigurationsdatei und starten Sie den Vokabularserver neu, damit die Änderungen wirksam werden.
