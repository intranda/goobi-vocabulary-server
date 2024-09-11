# Einrichtung
Diese Dokumentation beschreibt den Prozess der Installation und Ersteinrichtung des Vokabularservers.

## Download und Installation
- Laden Sie die [Neuste Version](https://github.com/intranda/goobi-vocabulary-server/releases/latest) des Vokabularservers herunter.
- Laden Sie die [Konfigurationsdatei](https://github.com/intranda/goobi-vocabulary-server/releases/latest/download/application.properties) des Vokabularservers herunter.
- Passen Sie die Konfigurationsdatei entsprechend Ihrer Konfiguration an und entfernen Sie nicht geänderte Zeilen.
    - Datenbankanmeldeinformationen und Datenbankname.
    - Basis-URL und Port.
    - Sicherheitstoken (dieses muss identisch auch in Goobi konfiguriert werden).
- Erstellen Sie ein Systemd-Service, um den Dienst automatisch zu starten.

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

## Installationsskript
Für die obigen drei Punkte unter Ubuntu:
```bash
export VOC_PORT=8081
export VOC_TOKEN=$(</dev/urandom tr -dc '[:alnum:]' | head -c17)
export VOC_PATH=/opt/digiverso/vocabulary
export VOC_USER=vocabulary
export VOC_SQL_USER=${VOC_USER}
export VOC_SQL_DB=${VOC_USER}
export PW_SQL_VOC=$(</dev/urandom tr -dc '[:alnum:]' | head -c17)

# create install folder
sudo mkdir ${VOC_PATH}
# download and link vocabulary server application file
wget https://github.com/intranda/goobi-vocabulary-server/releases/latest/download/vocabulary-server-core.jar -O - | sudo tee ${VOC_PATH}/vocabulary-server-core.jar >/dev/null

# create system user which will run the service
sudo adduser --system --home ${VOC_PATH}/home --shell /usr/sbin/nologin --no-create-home --disabled-login ${VOC_USER}

# download the vocabulary migration tools
git clone --depth=1 https://github.com/intranda/goobi-vocabulary-server.git /tmp/goobi-vocabulary-server
sudo cp -ait ${VOC_PATH} /tmp/goobi-vocabulary-server/migration

# download and set up the config file
wget https://github.com/intranda/goobi-vocabulary-server/releases/latest/download/application.properties -O - | sudo tee ${VOC_PATH}/application.properties >/dev/null
sudo sed -re "s|^(server.port=).*|\1${VOC_PORT}|" \
     -e "s|^(security.token=).*|\1${VOC_TOKEN}|" \
     -e "s|^(spring.datasource.username=).*|\1${VOC_SQL_USER}|" \
     -e "s|^(spring.datasource.password=).*|\1${PW_SQL_VOC}|" \
     -e "s|^(spring.datasource.url=).*|\1jdbc:mariadb://localhost:3306/${VOC_SQL_DB}|" \
     -i ${VOC_PATH}/application.properties
sudo chown ${VOC_USER}: ${VOC_PATH}/application.properties
sudo chmod 600 ${VOC_PATH}/application.properties

# install a systemd service unit file
cat << EOF | sudo tee /etc/systemd/system/vocabulary.service
[Unit]
Description=Goobi Vocabulary Server
After=mysql.service remote-fs.target
Requires=mysql.service remote-fs.target

[Service]
WorkingDirectory=${VOC_PATH}
Restart=always
RestartSec=20s
StartLimitInterval=100s
StartLimitBurst=4
ExecStart=/usr/bin/java -jar vocabulary-server-core.jar
User=${VOC_USER}
NoNewPrivileges=true
ProtectSystem=true
PrivateTmp=yes

[Install]
WantedBy=default.target tomcat9.service
EOF
sudo systemctl daemon-reload
sudo systemctl enable vocabulary.service

# create and configure the database
sudo mysql -e "CREATE DATABASE ${VOC_SQL_DB};
               CREATE USER '${VOC_SQL_USER}'@'localhost' IDENTIFIED BY '${PW_SQL_VOC}';
               GRANT ALL PRIVILEGES ON ${VOC_SQL_DB}.* TO '${VOC_SQL_USER}'@'localhost' WITH GRANT OPTION;
               FLUSH PRIVILEGES;"

# append vocabulary server address to the Goobi workflow config
grep ^vocabularyServerHost= /opt/digiverso/goobi/config/goobi_config.properties || echo "vocabularyServerHost=localhost"   | sudo tee -a /opt/digiverso/goobi/config/goobi_config.properties
grep ^vocabularyServerPort= /opt/digiverso/goobi/config/goobi_config.properties || echo "vocabularyServerPort=${VOC_PORT}" | sudo tee -a /opt/digiverso/goobi/config/goobi_config.properties
grep ^vocabularyServerToken= /opt/digiverso/goobi/config/goobi_config.properties || echo "vocabularyServerToken=${VOC_TOKEN}" | sudo tee -a /opt/digiverso/goobi/config/goobi_config.properties

# start the vocabulary server
sudo systemctl start vocabulary.service
## check startup
journalctl -u vocabulary.service -f

# initial set up
wget https://github.com/intranda/goobi-vocabulary-server/releases/latest/download/default_setup.sh -O - | sudo tee ${VOC_PATH}/default_setup.sh >/dev/null

# edit credentials
vim ${VOC_PATH}/default_setup.sh
bash ${VOC_PATH}/default_setup.sh
## test
curl -s http://localhost:${VOC_PORT}/api/v1/types --header "Authorization: Bearer $VOC_TOKEN" | jq -r '._embedded.fieldTypeList[] .name'
```
Der Vokabularserver benötigt Java 17, der Systemd-Service geht davon aus, dass Java 17 der System-Default ist.

## Erreichbarkeit
- Sie können den Vokabularserver von außen erreichbar machen, indem Sie einen Proxy samt Zugriffskontrolle davorschalten.

## Installationstest
- Ändern Sie für alle Befehle Host und Port entsprechend.
- Prüfen Sie nach der Ersteinrichtung, ob die Feldtypen erfolgreich erstellt wurden:
```bash
curl http://localhost:8081/api/v1/types --header "Authorization: Bearer $VOC_TOKEN" | jq -r '._embedded.fieldTypeList[] .name'
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
