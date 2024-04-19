package io.goobi.vocabulary.exception;

public class MissingAttributeException extends IllegalArgumentException {
    public MissingAttributeException(Class<?> type, String missingAttribute) {
        super("Missing required attribute \"" + missingAttribute + "\" for \"" + type.getSimpleName() + "\"");
    }
}
