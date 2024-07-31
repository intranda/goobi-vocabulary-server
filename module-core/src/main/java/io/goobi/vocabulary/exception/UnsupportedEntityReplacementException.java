package io.goobi.vocabulary.exception;

import java.util.Map;

import static io.goobi.vocabulary.exception.VocabularyException.ErrorCode.InsertingNonExistingItem;

public class UnsupportedEntityReplacementException extends VocabularyException {
    public UnsupportedEntityReplacementException(Class<?> type, long id) {
        super(InsertingNonExistingItem, null, Map.of("type", type.getSimpleName(), "id", String.valueOf(id)),
                params -> "Cannot insert non existing " + params.get("type") + " with id \"" + params.get("id") + "\", this behavior is not implemented yet");
    }
}
