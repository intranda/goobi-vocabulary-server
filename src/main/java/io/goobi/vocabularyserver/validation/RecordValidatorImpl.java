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
        super("Record");
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
        List<FieldDefinition> providedFields = vocabularyRecord.getFields()
                .stream()
                .map(FieldInstance::getDefinition)
                .collect(Collectors.toList());
        missingFields.removeAll(providedFields);
        if (!missingFields.isEmpty()) {
            throw new RecordValidationException(
                    "Missing required fields: "
                            + missingFields.stream()
                            .map(f -> "\"" + f.getName() + "\" [" + f.getId() + "]")
                            .collect(Collectors.joining(", ")) + " -- "
                            + "provided fields: " + providedFields.stream()
                            .map(f -> "\"" + f.getName() + "\" [" + f.getId() + "]")
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
            String errorMessages = errors.stream().map(Throwable::getMessage).collect(Collectors.joining("\n"));
            List<String> errorLines = List.of(errorMessages.split("\n"));
            throw new RecordValidationException(errorLines.stream()
                    .map(l -> "\t" + l)
                    .collect(Collectors.joining("\n")));
        }
    }
}
