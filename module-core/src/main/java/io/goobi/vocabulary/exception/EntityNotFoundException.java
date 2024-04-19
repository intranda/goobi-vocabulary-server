package io.goobi.vocabulary.exception;

public class EntityNotFoundException extends IllegalArgumentException {
    public EntityNotFoundException(Class<?> type, long id) {
        super(type.getSimpleName() + " with id \"" + id + "\" not found");
    }

    public EntityNotFoundException(Class<?> type, String searchTerm) {
        super(type.getSimpleName() + " for search term \"" + searchTerm + "\" not found");
    }
}
