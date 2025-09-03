---
title: Snippets
published: true
---
Below are some snippets that illustrate how the vocabulary server can be put into operation using the example of a Goobi-to-go instance:

## Test for content in the vocabulary server

```bash
curl "http://localhost:8081/api/v1/types" --header "Authorization: Bearer secret" | jq
curl "http://localhost:8081/api/v1/schemas" --header "Authorization: Bearer secret" | jq
curl "http://localhost:8081/api/v1/vocabularies" --header "Authorization: Bearer secret" | jq
```

## Downloading the installation script

```bash
wget https://github.com/intranda/goobi-vocabulary-server/releases/latest/download/vocabulary-init-script.zip
unzip vocabulary-init-script.zip
```

## Example of creating minimal types

```bash
HOST=localhost PORT=8081 TOKEN=secret vocabulary-init-script/install.sh minimal
```

## Example of creating vocabularies for books

```bash
HOST=localhost PORT=8081 TOKEN=secret vocabulary-init-script/install.sh books
```


## Example of creating vocabularies for collections

```bash
HOST=localhost PORT=8081 TOKEN=secret vocabulary-init-script/install.sh collections
```