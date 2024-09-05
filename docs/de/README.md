# Dokumentation
Dieses Dokument beschreibt den neuen Vokabularserver. Bis zur Version 24.06 waren Vokabulare Teil von Goobi Workflow und wurden in der Goobi-Datenbank gespeichert. Ab Version 24.07 ist alles, was mit Vokabularen zu tun hat, in eine eigenständige Anwendung, den Vokabularserver, umgezogen. Der Vokabularserver benötigt eine eigene Datenbank, um alle Daten zu speichern, und ermöglicht den Zugriff auf die Vokabulare und Datensätze über eine REST-API. Goobi Workflow wurde aktualisiert, um den neuen Vokabularserver anstelle seiner eigenen, eingebetteten Vokabulare zu verwenden. Falls gewünscht, kann der Vokabularserver öffentlich zugänglich sein. Wenn Sie bereits vorher Vokabulare verwendet haben, lesen Sie bitte die Migrationsanleitung in dieser Dokumentation, um Ihre Daten auf den neuen Vokabularserver zu übertragen.

## Installation
Bevor Sie den Vokabularserver nutzen können, folgen Sie den [Installations Anweisungen](setup.md).

## Vokabularerstellung
Vokabulare und Vokabularschemata sind ein komplexes Thema für sich, daher wird die [Dokumentation der Erstellung von Vokabularen, Schemata und Feldtypen](creation.md) separat behandelt.

## Datenmigration existierender Vokabulare
Wenn Sie bestehende Vokabulare aus früheren Versionen von Goobi migrieren wollen, folgen Sie bitte der [Dokumentation der Datenmigration](migration.md).
