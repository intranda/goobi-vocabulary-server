#!/usr/bin/env bash
MY_PATH="$(dirname -- "${BASH_SOURCE[0]}")"
SAMPLES_DIR=$MY_PATH/samples

# Check for parameters
FAIL=0
if [ -z $HOST ]; then
    echo "Missing \"HOST\" parameter!"
    FAIL=1
fi
if [ -z $PORT ]; then
    echo "Missing \"PORT\" parameter!"
    FAIL=1
fi
if [ -z $TOKEN ]; then
    echo "Missing \"TOKEN\" parameter!"
    FAIL=1
fi
if [ $FAIL == "1" ]; then
    echo "Please call this script like the following way (sample call):"
    echo "$ HOST=localhost PORT=8081 TOKEN=yoursecrettoken $MY_PATH/install_samples.sh"
    exit 1
fi

SAMPLE=$1
if [ -z $SAMPLE ]; then
    echo "Please chose a sample to install:"
    ls $SAMPLES_DIR
    read SAMPLE
fi

if [ -d "$SAMPLE" ]; then
    SAMPLE_PATH="$SAMPLE"
else
    SAMPLE_PATH="$SAMPLES_DIR/$SAMPLE"
fi
if [ ! -e $SAMPLE_PATH ]; then
    echo "Sample \"$SAMPLE\" does not exist!"
    exit 1
fi
CACHE_FILE="$SAMPLE_PATH/cache.txt"

curl_call() {
    curl --location "$HOST:$PORT/api/v1/$1" \
        --header 'Content-Type: application/json' \
        --header "Authorization: Bearer $TOKEN" \
        --fail \
        --silent \
        --data "$2"
}

curl_file_upload_call() {
    curl --location "$HOST:$PORT/api/v1/$1" \
        --header "Authorization: Bearer $TOKEN" \
        --fail \
        --silent \
        --form "file=@\"$2\""
}

for INSTALL_DIR in $(ls $SAMPLE_PATH); do
    ENDPOINT=$(echo $INSTALL_DIR | cut -d'_' -f2)
    echo "Installing $ENDPOINT"
    for ITEM in $(ls $SAMPLE_PATH/$INSTALL_DIR | grep ".json"); do
        ITEM_NAME=$(echo "$ITEM" | cut -d'.' -f1)
        ITEM_IDENTIFIER=${ENDPOINT}_"${ITEM_NAME}"

        if [ -f "$CACHE_FILE" ]; then
            ID=$(cat $CACHE_FILE | grep "$ITEM_IDENTIFIER;" | cut -d';' -f2)
        else
            ID=""
        fi
        if [ -z "$ID" ]; then
            JSON=$(cat $SAMPLE_PATH/$INSTALL_DIR/$ITEM)

            # Replace ID placeholders
            if [ -f "$CACHE_FILE" ]; then
                for CACHE_LINE in $(cat $CACHE_FILE); do
                    TEMPLATE_PLACEHOLDER="{{$(echo $CACHE_LINE | cut -d';' -f1)}}"
                    CACHE_ID=$(echo $CACHE_LINE | cut -d';' -f2)
                    JSON=$(echo $JSON | sed "s/$TEMPLATE_PLACEHOLDER/$CACHE_ID/g")
                done
            fi

            RESULT=$(curl_call $ENDPOINT "$JSON")
        
            if [ -z "$RESULT" ]; then
                echo "Error during creation, exiting!"
                echo $JSON
                exit 2
            fi
            ID=$(echo $RESULT | jq ".id")
            echo "$ITEM_IDENTIFIER;$ID" >> $CACHE_FILE
            echo -e "\tInstalled $ITEM_NAME with ID $ID"

            if [ $ENDPOINT = "schemas" ]; then
                echo $RESULT | jq -r ".definitions[] | \"${ITEM_IDENTIFIER}_definitions_\(.name);\(.id)\"" >> $CACHE_FILE
            fi
        else
            echo -e "\tSkipping $ITEM_NAME, already present with ID $ID"
        fi
    done
    for ITEM in $(ls $SAMPLE_PATH/$INSTALL_DIR | grep ".csv"); do
        VOCABULARY_NAME=$(echo "$ITEM" | cut -d'.' -f1)
        VOCABULARY_IDENTIFIER=vocabularies_${VOCABULARY_NAME}
        if [ -f "$CACHE_FILE" ]; then
            VOCABULARY_ID=$(cat $CACHE_FILE | grep $VOCABULARY_IDENTIFIER | cut -d';' -f2)
        else
            VOCABULARY_ID=""
        fi

        if [ ! -z "$VOCABULARY_ID" ]; then
            curl_file_upload_call "vocabularies/$VOCABULARY_ID/import/csv" "$SAMPLE_PATH/$INSTALL_DIR/$ITEM"
            echo -e "\tImported \"$VOCABULARY_NAME\" vocabulary records"
        else
            echo -e "\tSkipping record imports into \"$VOCABULARY_NAME\" vocabulary, vocabulary not created beforehand!"
        fi
    done
    for ITEM in $(ls $SAMPLE_PATH/$INSTALL_DIR | grep ".xlsx"); do
        VOCABULARY_NAME=$(echo "$ITEM" | cut -d'.' -f1)
        VOCABULARY_IDENTIFIER=vocabularies_${VOCABULARY_NAME}
        if [ -f "$CACHE_FILE" ]; then
            VOCABULARY_ID=$(cat $CACHE_FILE | grep $VOCABULARY_IDENTIFIER | cut -d';' -f2)
        else
            VOCABULARY_ID=""
        fi
        
        if [ ! -z "$VOCABULARY_ID" ]; then
            curl_file_upload_call "vocabularies/$VOCABULARY_ID/import/excel" "$SAMPLE_PATH/$INSTALL_DIR/$ITEM"
            echo -e "\tImported \"$VOCABULARY_NAME\" vocabulary records"
        else
            echo -e "\tSkipping record imports into \"$VOCABULARY_NAME\" vocabulary, vocabulary not created beforehand!"
        fi
    done
done
