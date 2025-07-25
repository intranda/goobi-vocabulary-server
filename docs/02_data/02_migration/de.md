---
title: Migration von Vokabularen
published: true
---
Diese Dokumentation führt Sie durch den Prozess der Migration bestehender Vokabulardaten auf den neuen Vokabularserver.
So können Sie überprüfen, ob bereits Vokabulare vorhanden sind: 

```bash
mysql goobi -e "select * from vocabulary;"
```

Wenn die Ausgabe einige Vokabulare auflistet, müssen Sie eine Migration durchführen, um diese Vokabulare beizubehalten.

Für alle folgenden Anweisungen muss der Vokabularserver bereits laufen.

## Anweisungen zur Einrichtung
Erstellen Sie zunächst eine virtuelle Python-Umgebung, aktivieren Sie diese und installieren Sie alle erforderlichen Python-Abhängigkeiten. Alle folgenden Anweisungen in dieser Dokumentation setzen immer eine aktivierte Python-Umgebung voraus, in der alle diese Abhängigkeiten vorhanden sind.

```bash
python3 -m venv vmenv
. vmenv/bin/activate
pip install wheel # This needs to be done beforehand as a separate call
pip install requests mysql-connector-python==8.4.0 alive_progress lxml
```

## Migration der Vokabulardaten durchführen
Laden Sie das [Vocabulary Migration Tool](https://github.com/intranda/goobi-vocabulary-server/releases/latest/download/migration-tool.zip) herunter und entpacken Sie es, falls nicht schon bei der Installation geschehen.

:::info
Bevor Sie einen der folgenden Schritte durchführen, lesen Sie diese Dokumentation bitte zuerst vollständig durch.
Es gibt keine einfache "Nur diese Schritte ausführen"-Lösung für jeden Anwendungsfall.
Je nachdem wie die bisherigen Daten und Feldtypen aussehen, können weitere Schritte erforderlich sein.
:::

Während der Datenmigration generiert das Migrationstool neue Feldtypen (bitte lesen Sie [die Anleitung zur manuellen Erstellung neuer Daten](/de/other/vocabulary/data/creation/), wenn Sie nicht wissen, was Feldtypen sind oder wie Vokabulare im Allgemeinen funktionieren).
Jedes bestehende Vokabularschema, das Felder mit auswählbaren Werten enthält, wird zu einem neuen Typ migriert, der diese auswählbaren Werte enthält.
Wenn Sie es vermeiden wollen, diese Feldtypen für jedes Vokabular neu zu erstellen, oder wenn Sie vorher komplexere Typen erstellen wollen, die in den migrierten Daten verwendet werden sollen, können Sie diese Feldtypen vor der Datenmigration manuell erstellen und diese Informationen an das Migrationsskript übergeben.
Durch Angabe des Parameters `lookup-file-directory` können Sie das Migrationsskript anweisen, vorhandene auswählbare Werte auf die angegebenen Feldtypen abzubilden, anstatt sofort neue zu erzeugen.
Detaillierte Anweisungen hierzu werden später gegeben.

Wenn Sie keine Feldtypen erstellen wollen, können Sie die Datenmigration mit dem folgenden Befehl starten:

```bash
python vocabulary-migrator.py --vocabulary-server-host localhost --vocabulary-server-port 8081 --vocabulary-server-token TOKEN --goobi-database-host localhost --goobi-database-port 3306 --goobi-database-name goobi --goobi-database-user goobi --goobi-database-password goobi --continue-on-error --fallback-language eng
```

### Skript
Die obigen beiden Puntke, die virtuelle Python-Umgebung und die Migration der Vokabulardaten in einer typischen Installation, als root:

```bash
cd /opt/digiverso/vocabulary/migration
python3 -m venv vmenv
. vmenv/bin/activate
pip install requests mysql-connector-python==8.4.0 alive_progress lxml
VOC_PORT=$(sudo grep -oP '^server.port=\K.*' /opt/digiverso/vocabulary/application.properties)
VOC_TOKEN=$(sudo grep -oP '^security.token=\K.*' /opt/digiverso/vocabulary/application.properties)
DB_GOOBI_PW=$(sudo xmlstarlet sel -t -v '//Resource/@password' -n /etc/tomcat9/Catalina/localhost/goobi.xml)
python vocabulary-migrator.py --vocabulary-server-host localhost --vocabulary-server-port "${VOC_PORT}" --vocabulary-server-token "${VOC_TOKEN}" --goobi-database-host localhost --goobi-database-port 3306 --goobi-database-name goobi --goobi-database-user goobi --goobi-database-password "${DB_GOOBI_PW}" --continue-on-error --fallback-language ger

# Test
curl -s http://localhost:8081/api/v1/vocabularies --header "Authorization: Bearer $VOC_TOKEN" | jq -r '._embedded.vocabularyList[] .name'
```

**Hinweis** Ändern Sie die Parameter entsprechend Ihrer Konfiguration. Der Parameter `fallback-language` definiert die Standardsprache, die für ein mehrsprachiges Vokabularfeld verwendet wird, für das keine Standardsprache abgeleitet werden konnte. Die Option `continue-on-error` verhindert, dass das Migrationstool bei Fehlern bei der Datenmigration anhält. Diese Fehler können auftreten, wenn die Daten nicht in den neuen Vokabularserver eingefügt werden konnten. Mögliche Gründe dafür könnten sein:
- Der Vokabulardatensatz ist leer.
- Der Vokabulardatensatz enthält Daten, die mit einigen Typbeschränkungen nicht kompatibel sind.
- Andere, hier nicht genannte Gründe.
- Nachdem die Datenmigration abgeschlossen ist, überprüfen Sie die Datei `migration_issues.log` auf alle fehlgeschlagenen Datensatzmigrationen:
    - Führen Sie `cat migration_issues.log | grep "Error validating Record" | wc -l` aus, um die Anzahl der fehlgeschlagenen Datensatzmigrationen zu ermitteln.
    - Wenn der Datensatz keine nützlichen Daten enthält (leerer Datensatz, defekte Daten), ignorieren Sie das Problem.
    - Wenn der Datensatz gültige Informationen enthält, prüfen Sie den Grund für die fehlgeschlagene Datenmigration und versuchen Sie, ihn in der ursprünglichen `goobi`-Datenbank zu beheben (für einen erneuten Import) oder fügen Sie den neuen Datensatz später manuell hinzu.
    - Wenn der Datensatz gültige Informationen enthält und Sie nicht in der Lage sind, ihn so zu ändern, dass er funktioniert, wenden Sie sich an den Support.
    - Wenn Sie aufgrund von Migrationsproblemen Datensätze angepasst haben, gehen Sie wie folgt vor, um die Daten erneut zu importieren:
        - Fahren Sie den Vokabularserver herunter.
        - Leeren Sie die Datenbank des Vokabularservers.
        - Starten Sie den Vokabularserver.
        - Führen Sie die Ersteinrichtung durch.
        - Führen Sie die Vokabular-Migration wie oben beschrieben durch.
- Speichern Sie nach Abschluss der Migration die Datei `migration.csv` an einem sicheren Ort.
Diese Datei enthält alle Informationen über die migrierten Datensätze mit ihren alten und neuen IDs.
Diese Informationen werden benötigt, um später die Verweise auf die Vokabulardatensätze in den Metadaten-Dateien zu aktualisieren.
- Sie sollten versuchen, die Migration am Ende ohne den Parameter `--continue-on-error` durchzuführen. Wenn dies funktioniert, dann wissen Sie dass alles problemslos migriert werden konnte.

### Beispiel
Anhand des folgenden Beispiels möchten wir ein mögliches Migrationsproblem betrachten (aus der Datei `migration_issues.log`):

```json
---------------------------------------------------------------------------------
29293
[(1441967, 29293, 1, 1, 'original value', '', 'some-value'), (1441968, 29293, 1, 2, 'corrected value', '', 'some-other-value'), (1441969, 29293, 1, 3, 'type', '', 'some-type'), (1441970, 29293, 1, 4, 'authority', '', 'Geonames'), (1441971, 29293, 1, 5, 'authorityURI', '', 'http://www.geonames.org/'), (1441972, 29293, 1, 25, 'valueURI', '', 'some-uri')]
API call was not successful, reason:
Validation error
Error validating record fields
Validation error
Error validating field value, reason(s):
Validation error
Validation error
Field values(s) "Geonames" are not one of the allowed selectable values for field "authority" [100775]: geonames,viaf
---------------------------------------------------------------------------------
```

Dieses Problem zeigt an, dass eines der Vokabulardatensatzfelder den Wert `Geonames` enthält.
Während der Migration wurde dieses Feld (auf der Grundlage vorhandener Daten) mit einem neuen Typ konfiguriert, der die folgenden auswählbaren Werte `geonames` und `viaf` enthält.
Wie Sie sehen können, wird der aktuelle Wert mit einem großen `G` geschrieben, aber nur die klein geschriebene Version von `geonames` ist einer der auswählbaren Werte.
Daher schlägt die Validierung dieses Vokabeldatensatzes fehl und das Skript ist nicht in der Lage, diesen Datensatz zu importieren.
In diesem speziellen Fall könnten Sie alle Vorkommen in der alten Datenbank aktualisieren und anschließend einen erneuten Import der Daten durchführen. Leeren Sie dazu zunächst die Vokabular-(SQL)-Datenbank.
Stellen Sie sicher, dass Sie die Datei `migration_issues.log` vor einem Re-Import umbenennen oder entfernen, da das Migrationsscript immer an diese Datei anhängt und so bereits gelöste Probleme enthalten würde.
Jeder Vokabulardatensatz, der in dieser Datei gemeldet wird, wurde nicht in den Vokabularserver importiert.
Wenn dies gewünscht ist (weil der Vokabeldatensatz fehlerhafte Daten enthält und nicht beibehalten werden soll), können Sie dieses Problem einfach ignorieren und weitermachen.

Mit `python vocabulary-migrator.py --help` können Sie sich alle verfügbaren Optionen des Migrationsscripts anzeigen lassen.

### Der `lookup-file-directory` Parameter
Wenn Sie vorhaben, bestehende Feldtypen wiederzuverwenden oder andere Vokabulare für Vokabularreferenzen während einer Migration zu verwenden, erstellen Sie die folgenden drei Dateien: `reference_type_lookup.csv`, `reference_value_lookup.csv` und `type_definition_lookup.csv`.
Legen Sie alle drei Dateien in einem neuen Verzeichnis ab und übergeben Sie diesen Verzeichnispfad als Parameter `--lookup-file-directory` an das Migrationstool.

Um die folgende Konfiguration besser zu verstehen, geben wir ein Beispiel.
Stellen Sie sich vor, Sie haben derzeit ein Vokabular mit einem Feld mit den folgenden auswählbaren Werten: `red`, `blue`.
Datensätze können jeden dieser beiden Werte enthalten.
Die Unterstützung mehrerer Sprachen wird derzeit erreicht, indem eine neue Felddefinition mit einem anderen Sprachwert erstellt wird und die gleiche Anzahl von auswählbaren Werten, diesmal in der anderen Sprache, bereitgestellt wird: `rot`, `blau` (auf Deutsch).
In der neuen Vokabularverwaltung können Sie diese Farbangaben in ein neues Vokabular verschieben und alle Datensätze, die Farbwerte enthalten, durch Verweise auf dieses Vokabular ersetzen.Das macht es viel einfacher, Farben hinzuzufügen und ihre Übersetzungen im Auge zu behalten.
Damit dies funktioniert, müssen wir zunächst ein neues Vokabular `Farben` erstellen und zwei Datensätze für eine rote und eine blaue Farbe hinzufügen. In diesem Beispiel hat das Vokabular die ID `2`. Beide Datensätze enthalten sowohl englische als auch deutsche Übersetzungen.
In diesem Beispiel hat der Datensatz für die rote Farbe die ID `122` und der für die blaue Farbe die ID `123`.

Außerdem wollen wir einen neuen Feldtypen einführen, der die Werte `an` und `aus` annehmen kann.
Dieser Typ soll immer dann verwendet werden, wenn die Datensätze diese auswählbaren Werte enthalten.
In diesem Beispiel hat der erstellte Feldtyp die ID `21`.

Lassen Sie uns nun die drei für dieses Beispiel erforderlichen Dateien erstellen:

Die Datei `type_definition_lookup.csv` sieht wie folgt aus:

```csv
values,type_id
an|aus,21
```

Diese Datei ordnet alle auswählbaren Werte in der ursprünglichen Felddefinition die neue Feldtyp-ID zu.

Die Datei `reference_type_lookup.csv` sieht wie folgt aus:

```csv
values,vocabulary_id
red|blue,2
rot|blau,2
```

Diese Datei ordnet allen auswählbaren Werten (in allen Sprachen, eine Sprache pro Zeile) die ID des Vokabulars zu, welches die Farbdatensätze enthält.
Achten Sie bitte darauf, immer eine Sprache pro Zeile zu definieren.
Der Grund dafür ist, dass verschiedene Sprachen vorher als mehrere Felddefinitionen vorhanden waren und die Migrationsverarbeitung diese Art der Trennung erfordert.
Der Wert für die Spalten `values` muss mit der Spalte `selection` in der Tabelle `vocabulary_structure` der bestehenden Datenbank übereinstimmen.

Die Datei `reference_value_lookup.csv` sieht wie folgt aus:

```csv
value,record_id
red,122
rot,122
blue,123
blau,123
```

Diese Datei ordnet allen Datensatzwerten die entsprechende Datensatz-IDs im Referenzvokabular zu (also die ID des Farbdatensatzes im Farbvokabular).

### Test der Vokabulardaten-Migration
- Wenn eine Datenmigration stattgefunden hat, prüfen Sie, ob alle Vokabulare migriert wurden:

```bash
curl -s http://localhost:8081/api/v1/vocabularies --header "Authorization: Bearer $VOC_TOKEN" | jq -r '._embedded.vocabularyList[] .name'
```

- Prüfen Sie, ob die Links korrekt aufgelöst werden (siehe Konfiguration):

```bash
curl http://localhost:8081/api/v1/records/1 --header "Authorization: Bearer $VOC_TOKEN" | jq
```

Das JSON-Element `_links` sollte Verweise auf andere Ressourcen enthalten.
Diese URLs sollten gültig und auflösbar sein.
Der Host-Teil dieser URLs wird aus der Anfrage generiert.

## Migration der Mets-Datei

:::warning
Bitte erstellen Sie vorher ein Backup. In einigen Fällen sind die mets-Dateien inkonsistent, d. h. die gespeicherten Werte und Vokabularreferenzen stimmen nicht überein. Das Migrationsskript verwendet die Vokabularreferenzen, um die richtigen Werte zu finden. Wenn die Vokabularreferenzen falsch und die Werte richtig sind, wird die Migration die Datenintegrität korrumpieren!
:::

Dieser Schritt kann nur durchgeführt werden, wenn die Migration der Vokabulardaten erfolgreich abgeschlossen wurde!

Wenn die Datei `migration.csv` vorhanden ist, führen Sie den folgenden Befehl in der aktivierten Python-Umgebung aus:

```bash
cd /opt/digiverso/vocabulary/migration
sudo -s
. vmenv/bin/activate
# dry-run:
python metadata-migrator.py --vocabulary-server-token "${VOC_TOKEN}" --verbose --log INFO -m migration.csv -d /opt/digiverso/goobi/metadata --dry
# metadata migration
python metadata-migrator.py --vocabulary-server-token "${VOC_TOKEN}" -m migration.csv -d /opt/digiverso/goobi/metadata
```

Dadurch werden alle mets-Dateien in allen Prozessverzeichnissen rekursiv verarbeitet.
Wann immer das Script eine Vokabularreferenz in der Mets-Datei findet, wird es versuchen, diese Referenz durch die aktualisierten Informationen zu ersetzen.
Wenn etwas geändert wird, wird zuvor ein Backup der Mets-Datei erstellt.

Wenn die Mets-Dateien zusätzliche Referenzen auf Datensätze in separaten XML-Elementen enthalten (z. B. `<goobi:metadata name="SourceID">5661</goobi:metadata>`), kann das `metadata-migrator.py` Script diese Referenzen auch mit dem zusätzlichen Parameter `--manual-id-fix SourceID` aktualisieren. Der Wert des Parameters muss mit dem Attribut `Name` eines `Metadaten`-Elements übereinstimmen, damit dessen Datensatz-ID durch die neue Datensatz-ID ersetzt werden kann. Dieser Schritt darf nicht zweimal ausgeführt werden, da dies die IDs verfälschen würde!

## Datenbereinigung
Wenn die Datenmigration erfolgreich abgeschlossen ist und Sie sicher sind, dass Sie die alten Daten nicht mehr benötigen, können Sie alle Vokabulartabellen manuell aus der Datenbank `goobi` Ihrer Goobi-Instanz entfernen:
- `vocabulary`
- `vocabulary_record`
- `vocabulary_record_data`
- `vocabulary_structure`

:::warning
Die Datenbereinigung kann nicht rückgängig gemacht werden.
Wenn Sie sich nicht sicher sind, führen Sie die Bereinigungsschritte nicht durch.
Die alten Vokabulardaten haben keinen Einfluss auf neuere Versionen von Goobi Workflow.
Wir empfehlen, diese Daten für den Fall der Fälle für einige Zeit aufzubewahren.
:::