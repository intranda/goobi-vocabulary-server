package io.goobi.vocabulary.exception;

import java.util.Map;

public class IllegalAttributeProvidedException extends VocabularyException {
    public IllegalAttributeProvidedException(String attribute) {
        super(ErrorCode.IllegalAttributeProvided, null, Map.of("attribute", attribute), (params) -> "You are not allowed to provide the attribute \"" + params.get("attribute") + "\"");
    }

    public IllegalAttributeProvidedException(String attribute, String hint) {
        super(ErrorCode.IllegalAttributeProvided, null, Map.of("attribute", attribute), (params) -> "You are not allowed to provide the attribute \"" + params.get("attribute") + "\", " + hint);
    }
}
