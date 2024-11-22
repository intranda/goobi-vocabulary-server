# Setup
This documentation describes the process of bootstrapping the vocabulary server.

## Download and Installation
- Download [Latest Build](https://github.com/intranda/goobi-vocabulary-server/releases/latest) of vocabulary server.
- Download [Configuration File](https://github.com/intranda/goobi-vocabulary-server/releases/latest/download/application.properties) of the vocabulary server.
- Adapt configuration file properly and remove unmodified lines.
    - Database credentials and database name.
    - Base URL and port.
    - Security token (this must also be configured identically in Goobi).
- Create a Systemd service to start the service automatically. 

## Setting up Goobi Workflow to communicate with the vocabulary server

- Goobi Workflow has been using the new vocabulary server since version `24.07`.
- Configure the variables `vocabularyServerHost`, `vocabularyServerPort` and `vocabularyServerToken` in the `goobi_config.properties` file according to the configuration of your vocabulary server.
- Restart Goobi Workflow for the changes to take effect.
- Navigate to `Administration` > `Vocabularies` to check that everything is working. You should see a list of vocabularies if everything is OK (not now, but after you have created some vocabularies or migrated the existing ones). If something is not working, you will see a red error message.


## Initial setup

- For proper operation, the vocabulary server requires some initial data.
- This data contains language information (if multilingual vocabularies are used) and field type definitions.
- You can use the following script, which installs some sample languages and field types.
- Download the [Initial Data Script](https://github.com/intranda/goobi-vocabulary-server/raw/develop/install/default_setup.sh).
- Change the variables `HOST` and `TOKEN` at the beginning according to the configuration of the vocabulary server, leave the suffix `/api/v1` unchanged.
- Execute the script.

## Installation script
The vocabulary server requires Java 17, the systemd service assumes that Java 17 is the system default.

For the above three points, under Ubuntu:
``bash
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
wget https://github.com/intranda/goobi-vocabulary-server/releases/latest/download/migration-tool.zip -O /tmp/migration-tool.zip
sudo unzip /tmp/migration-tool.zip -d "${VOC_PATH}"

# download and set up the config file
wget https://github.com/intranda/goobi-vocabulary-server/releases/latest/download/application.properties -O - | sudo tee ${VOC_PATH}/application.properties >/dev/null
sudo sed -re "s|^(server.port=).*|\1${VOC_PORT}|" \
     -e "s|^#?(security.token=).*|\1${VOC_TOKEN}|" \
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
sudo mysql -e "CREATE DATABASE ${VOC_SQL_DB} CHARACTER SET = 'utf8mb4' COLLATE = 'utf8mb4_unicode_ci';
               CREATE USER '${VOC_SQL_USER}'@'localhost' IDENTIFIED BY '${PW_SQL_VOC}';
               GRANT ALL PRIVILEGES ON ${VOC_SQL_DB}.* TO '${VOC_SQL_USER}'@'localhost' WITH GRANT OPTION;
               FLUSH PRIVILEGES;"

# append vocabulary server address to the Goobi workflow config
grep ^vocabularyServerHost= /opt/digiverso/goobi/config/goobi_config.properties || echo "vocabularyServerHost=localhost"   | sudo tee -a /opt/digiverso/goobi/config/goobi_config.properties
grep ^vocabularyServerPort= /opt/digiverso/goobi/config/goobi_config.properties || echo "vocabularyServerPort=${VOC_PORT}" | sudo tee -a /opt/digiverso/goobi/config/goobi_config.properties
grep ^vocabularyServerToken= /opt/digiverso/goobi/config/goobi_config.properties || echo "vocabularyServerToken=${VOC_TOKEN}" | sudo tee -a /opt/digiverso/goobi/config/goobi_config.properties

# start the vocabulary server and wait for startup
sudo systemctl restart vocabulary.service & sudo journalctl -u vocabulary.service  -f -n 0 | grep -q "Started VocabularyServerApplication in"

# initial set up
wget https://github.com/intranda/goobi-vocabulary-server/raw/develop/install/default_setup.sh -O /tmp/default_setup.sh
bash /tmp/default_setup.sh

## test
curl -s http://localhost:${VOC_PORT}/api/v1/types --header "Authorization: Bearer $VOC_TOKEN" | jq -r '._embedded.fieldTypeList[] .name'
```

## Accessibility
- You can make the vocabulary server accessible from outside by connecting a proxy with access control in front of it.

## Installation test
- Change the host and port accordingly for all commands.
- After the initial setup, check whether the field types have been created successfully:
```bash
curl ‘http://localhost:${VOC_PORT:-8081}/api/v1/types’ --header ‘Authorisation: Bearer $VOC_TOKEN’ | jq -r ‘._embedded.fieldTypeList[] .name’
```

- The result should look like this:
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
