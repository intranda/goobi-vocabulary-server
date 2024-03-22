package io.goobi.vocabularyserver.exception;

public class EntityNotFoundException extends IllegalArgumentException {
    public EntityNotFoundException(Class<?> type, long id) {
        super(type.getSimpleName() + " with id \"" + id + "\" not found");
    }
}
