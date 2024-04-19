package io.goobi.vocabulary.exception;

public class IllegalAttributeProvidedException extends IllegalArgumentException {
    public IllegalAttributeProvidedException(String attribute) {
        super("You are not allowed to provide the attribute \"" + attribute + "\"");
    }

    public IllegalAttributeProvidedException(String attribute, String hint) {
        super("You are not allowed to provide the attribute \"" + attribute + "\", " + hint);
    }
}
