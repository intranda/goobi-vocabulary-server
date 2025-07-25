---
title: Vokabularserver
published: true
sidemenu: true
description: Dokumentation für die Installation und Nutzung des Vokabularserver zur Anbindung an Goobi
---

## Über dieses Handbuch

Dieses Dokument beschreibt den Goobi Vokabularserver. Bis zur Version 24.06 waren Vokabulare Teil von Goobi Workflow und wurden in der Goobi-Datenbank gespeichert. Ab Version 24.07 ist alles, was mit Vokabularen zu tun hat, in eine eigenständige Anwendung, den Vokabularserver, umgezogen. Der Vokabularserver benötigt eine eigene Datenbank, um alle Daten zu speichern, und ermöglicht den Zugriff auf die Vokabulare und Datensätze über eine REST-API. Goobi Workflow wurde aktualisiert, um den neuen Vokabularserver anstelle seiner eigenen, eingebetteten Vokabulare zu verwenden. Falls gewünscht, kann der Vokabularserver öffentlich zugänglich sein. Wenn Sie bereits vorher Vokabulare verwendet haben, lesen Sie bitte die Migrationsanleitung in dieser Dokumentation, um Ihre Daten auf den neuen Vokabularserver zu übertragen.

Der Quelltext des Vokabularservers befindet sich auf GitHub.

[https://github.com/intranda/goobi-vocabulary-server](https://github.com/intranda/goobi-vocabulary-server)

## Ansprechpartner

Zu Fragen rund um diese Dokumentation, für Anregungen zum weiteren Ausbau dieses Handbuchs sowie zu allgemeinen Fragen zu Goobi, Digitalisierungsprojekten im Allgemeinen und natürlich auch bezüglich der Weiterentwicklung von Goobi, wenden Sie sich bitte gerne jederzeit an die intranda GmbH:

| Kontakt |  |
| :--- | :--- |
| Anschrift: | intranda GmbH, Bertha-von-Suttner Str. 9, D-37085 Göttingen |
| Telefon: | +49 551 291 76 100 |
| E-Mail: | [info@intranda.com](mailto:info@intranda.com) |
| URL: | [https://www.intranda.com](https://www.intranda.com) |

## Urheberrechte

Bitte beachten Sie, dass die vorliegende Dokumentation nicht verändert oder in veränderter Form weitergegeben werden darf. Eine kommerzielle Nutzung dieser Dokumentation ist nicht gestattet.

:::info
![copyright](icon_cc.png) 

Dieses Werk ist unter einer Creative Commons Lizenz vom Typ Namensnennung - Nicht kommerziell - Keine Bearbeitungen 4.0 International zugänglich. Um eine Kopie dieser Lizenz einzusehen, konsultieren Sie [http://creativecommons.org/licenses/by-nc-nd/4.0/](http://creativecommons.org/licenses/by-nc-nd/4.0/) oder wenden Sie sich brieflich an Creative Commons, Postfach 1866, Mountain View, California, 94042, USA.
:::


