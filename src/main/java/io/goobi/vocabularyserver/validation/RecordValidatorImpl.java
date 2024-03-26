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
public class RecordValidatorImpl extends BaseValidator<VocabularyRecord> {
    private final Validator<FieldInstance> fieldValidator;

    public RecordValidatorImpl(Validator<FieldInstance> fieldValidator) {
        super("Field validator");
        this.fieldValidator = fieldValidator;
        setValidations(List.of(
                this::checkRequiredFieldsExistence,
                this::perFieldInstanceChecks
        ));
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

    private void perFieldInstanceChecks(VocabularyRecord vocabularyRecord) throws RecordValidationException {
        List<Throwable> errors = new LinkedList<>();
        for (FieldInstance f : vocabularyRecord.getFields()) {
            try {
                fieldValidator.validate(f);
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
