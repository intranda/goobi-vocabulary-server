package io.goobi.vocabulary.exception;

public class UnsupportedEntityReplacementException extends IllegalArgumentException {
    public UnsupportedEntityReplacementException(Class<?> type, long id) {
        super("Cannot insert non existing " + type.getSimpleName() + " with id \"" + id + "\", this behavior is not implemented yet");
    }
}
