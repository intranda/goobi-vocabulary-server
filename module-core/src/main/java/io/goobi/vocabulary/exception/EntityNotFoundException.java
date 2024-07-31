package io.goobi.vocabulary.exception;

import java.util.Map;

public class EntityNotFoundException extends VocabularyException {
    public EntityNotFoundException(Class<?> type, long id) {
        super(ErrorCode.EntityNotFound, null, Map.of("type", type.getSimpleName(), "id", String.valueOf(id)),
                (params) -> params.get("type") + " with id \"" + params.get("id") + "\" not found");
    }

    public EntityNotFoundException(Class<?> type, String searchTerm) {
        super(ErrorCode.EntityNotFound, null, Map.of("type", type.getSimpleName(), "searchTerm", searchTerm),
                (params) -> params.get("type") + " for search term \"" + params.get("searchTerm") + "\" not found");
    }

    public EntityNotFoundException(Class<?> type) {
        super(ErrorCode.EntityNotFound, null, Map.of("type", type.getSimpleName()),
                (params) -> params.get("type") + " not found");
    }
}
