package io.goobi.vocabulary.validation;

import io.goobi.vocabulary.exception.ValidationException;

public interface Validator<T> {
    void validate(T t) throws ValidationException;
}
