package io.goobi.vocabularyserver.exception;

import java.util.List;

public class MissingValuesException extends IllegalArgumentException {
    public MissingValuesException(String type, List<String> possibleValues) {
        super("Missing values for \"" + type + "\", possible values are: " + String.join(", ", possibleValues));
    }
}
