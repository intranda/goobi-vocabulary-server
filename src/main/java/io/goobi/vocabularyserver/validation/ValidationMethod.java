package io.goobi.vocabularyserver.validation;

import io.goobi.vocabularyserver.exception.ValidationException;

public interface ValidationMethod<T> {
    void validate(T t) throws ValidationException;
}
