package io.goobi.vocabularyserver.validation;

import io.goobi.vocabularyserver.exception.FieldValidationException;
import io.goobi.vocabularyserver.model.FieldInstance;

public interface FieldValidator {
    void validate(FieldInstance field) throws FieldValidationException;
}
