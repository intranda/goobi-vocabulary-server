package io.goobi.vocabularyserver.exception;

import java.util.List;

public class MissingValuesException extends IllegalArgumentException {
    public MissingValuesException(Class<?> type, List<String> possibleValues) {
        super("Missing values for \"" + type.getSimpleName() + "\", possible values are: " + String.join(", ", possibleValues));
    }
}
