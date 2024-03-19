package io.goobi.vocabularyserver.exception;

public class EntityNotFoundException extends IllegalArgumentException {
    public EntityNotFoundException(String type, long id) {
        super(type + " with id \"" + id + "\" not found");
    }
}
