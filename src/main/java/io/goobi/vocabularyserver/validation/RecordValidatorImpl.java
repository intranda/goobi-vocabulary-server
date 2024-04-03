package io.goobi.vocabularyserver.validation;

import io.goobi.vocabularyserver.exception.RecordValidationException;
import io.goobi.vocabularyserver.exception.ValidationException;
import io.goobi.vocabularyserver.model.FieldDefinition;
import io.goobi.vocabularyserver.model.FieldInstance;
import io.goobi.vocabularyserver.model.VocabularyRecord;
import org.springframework.stereotype.Service;

import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

@Service
public class RecordValidatorImpl extends BaseValidator<VocabularyRecord> {
    private final Validator<FieldInstance> fieldValidator;

    public RecordValidatorImpl(Validator<FieldInstance> fieldValidator) {
        super("Record");
        this.fieldValidator = fieldValidator;
        setValidations(List.of(
                this::checkRequiredFieldsExistence,
                this::checkOnlyAllowedFieldDefinitionsGiven,
                this::checkHierarchy,
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

    private void checkOnlyAllowedFieldDefinitionsGiven(VocabularyRecord vocabularyRecord) throws RecordValidationException {
        List<FieldDefinition> providedFields = vocabularyRecord.getFields()
                .stream()
                .map(FieldInstance::getDefinition)
                .collect(Collectors.toList());
        providedFields.removeAll(vocabularyRecord.getVocabulary().getSchema().getDefinitions());
        if (!providedFields.isEmpty()) {
            throw new RecordValidationException(
                    "Non-defined fields given: "
                            + providedFields.stream()
                            .map(f -> "\"" + f.getName() + "\" [" + f.getId() + "]")
                            .collect(Collectors.joining(", "))
            );
        }
    }

    private void checkHierarchy(VocabularyRecord vocabularyRecord) throws RecordValidationException {
        if (Boolean.FALSE.equals(vocabularyRecord.getVocabulary().getSchema().getHierarchicalRecords())) {
            Set<String> errors = new HashSet<>();
            if (vocabularyRecord.getParentRecord() != null) {
                errors.add("can't define parent record if hierarchical records are deactivated in vocabulary schema");
            }
            if (!vocabularyRecord.getChildren().isEmpty()) {
                errors.add("can't define children records if hierarchical records are deactivated in vocabulary schema");
            }
            if (!errors.isEmpty()) {
                throw new RecordValidationException("Record violates hierarchy validation:\n\t- " + String.join("\n\t- ", errors));
            }
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
            throw new RecordValidationException(errorMessages);
        }
    }
}
