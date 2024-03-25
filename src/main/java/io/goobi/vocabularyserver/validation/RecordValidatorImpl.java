package io.goobi.vocabularyserver.validation;

import io.goobi.vocabularyserver.exception.RecordValidationException;
import io.goobi.vocabularyserver.exception.ValidationException;
import io.goobi.vocabularyserver.model.FieldDefinition;
import io.goobi.vocabularyserver.model.FieldInstance;
import io.goobi.vocabularyserver.model.VocabularyRecord;
import org.springframework.stereotype.Service;

import java.util.LinkedList;
import java.util.List;
import java.util.stream.Collectors;

@Service
public class RecordValidatorImpl implements Validator<VocabularyRecord> {
    private final Validator<FieldInstance> fieldValidator;
    private final List<ValidationMethod<VocabularyRecord>> validations;

    public RecordValidatorImpl(Validator<FieldInstance> fieldValidator) {
        this.fieldValidator = fieldValidator;
        validations = List.of(this::checkRequiredFieldsExistence);
    }

    private void checkRequiredFieldsExistence(VocabularyRecord vocabularyRecord) throws RecordValidationException {
        List<FieldDefinition> missingFields = vocabularyRecord.getVocabulary()
                .getSchema()
                .getDefinitions()
                .stream()
                .filter(FieldDefinition::getRequired)
                .collect(Collectors.toList());
        missingFields.removeAll(vocabularyRecord.getFields().stream().map(FieldInstance::getDefinition).collect(Collectors.toList()));
        if (!missingFields.isEmpty()) {
            throw new RecordValidationException("Missing required fields: "
                    + missingFields.stream()
                    .map(f -> "\"" + f.getName() + "\"")
                    .collect(Collectors.joining(", "))
            );
        }
    }

    @Override
    public void validate(VocabularyRecord vocabularyRecord) throws RecordValidationException {
        List<Throwable> errors = new LinkedList<>();
        for (FieldInstance f : vocabularyRecord.getFields()) {
            try {
                validator.validate(f);
            } catch (ValidationException e) {
                errors.add(e);
            }
        }
        for (ValidationMethod<VocabularyRecord> v : validations) {
            try {
                v.validate(vocabularyRecord);
            } catch (ValidationException e) {
                errors.add(e);
            }
        }
        if (!errors.isEmpty()) {
            throw new RecordValidationException("Error validating Vocabulary record:\n\t"
                    + errors.stream()
                    .map(Throwable::getMessage)
                    .collect(Collectors.joining("\n\t")));
        }
    }
}
