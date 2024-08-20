package io.goobi.vocabulary.exception;

import java.util.Map;

public class MappingException extends VocabularyException {
    public MappingException(Class<?> from, Class<?> to, Throwable e) {
        super(ErrorCode.Mapping, null, Map.of("from", from.getSimpleName(), "to", to.getSimpleName(), "reason", e.getMessage()),
                params -> "Unable to map \"" + params.get("from") + "\" to \"" + params.get("to") + "\". Reason: " + params.get("reason"));
    }
}
