package io.goobi.vocabulary.validation;

import io.goobi.vocabulary.exception.VocabularyException;

public interface Validator<T> {
    void validate(T t) throws VocabularyException;
}
