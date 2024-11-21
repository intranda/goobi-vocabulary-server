FROM eclipse-temurin:17-jre

ENV VOCABULARY_DB_SERVER vocabulary-db
ENV VOCABULARY_DB_USER vocabulary
ENV VOCABULARY_DB_PASSWORD vocabulary
ENV VOCABULARY_DB_DATABASE vocabulary
ENV VOCABULARY_SERVER_TOKEN secret
ENV VOCABULARY_SERVER_PORT 8081

RUN mkdir -p /opt/digiverso/vocabulary
COPY module-core/target/vocabulary-server-core.jar /opt/digiverso/vocabulary/
COPY module-core/src/main/resources/application.properties /opt/digiverso/vocabulary/
RUN sed -re "s|^(server.port=).*|\1${VOCABULARY_SERVER_PORT}|" \
     -e "s|^#?(security.token=).*|\1${VOCABULARY_SERVER_TOKEN}|" \
     -e "s|^#?(server.forward-headers-strategy.*)|\1|" \
     -e "s|^(spring.datasource.username=).*|\1${VOCABULARY_DB_USER}|" \
     -e "s|^(spring.datasource.password=).*|\1${VOCABULARY_DB_PASSWORD}|" \
     -e "s|^(spring.datasource.url=).*|\1jdbc:mariadb://$VOCABULARY_DB_SERVER:3306/${VOCABULARY_DB_DATABASE}|" \
     -i /opt/digiverso/vocabulary/application.properties

EXPOSE $VOCABULARY_SERVER_PORT
WORKDIR /opt/digiverso/vocabulary
CMD [ "java", "-jar", "vocabulary-server-core.jar"]
