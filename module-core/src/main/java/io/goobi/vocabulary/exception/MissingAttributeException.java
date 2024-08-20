package io.goobi.vocabulary.exception;

import java.util.List;
import java.util.Map;

public class MissingAttributeException extends VocabularyException {
    public MissingAttributeException(Class<?> type, List<String> missingAttributes) {
        this(type, String.join(",", missingAttributes));
    }

    public MissingAttributeException(Class<?> type, String missingAttribute) {
        super(ErrorCode.MissingRequiredAttribute, null, Map.of("type", type.getSimpleName(), "attributes", missingAttribute),
                params -> "Missing required attribute(s) \"" + params.get("attributes") + "\" for \"" + params.get("type") + "\"");
    }
}
