package io.goobi.vocabularyserver.validation;

import io.goobi.vocabularyserver.exception.RecordValidationException;
import io.goobi.vocabularyserver.model.VocabularyRecord;

public interface RecordValidator {
    void validate(VocabularyRecord record) throws RecordValidationException;
}
