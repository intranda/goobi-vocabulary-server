package io.goobi.vocabularyserver.validation;

import io.goobi.vocabularyserver.exception.RecordValidationException;
import io.goobi.vocabularyserver.exception.ValidationException;
import io.goobi.vocabularyserver.model.FieldDefinitionEntity;
import io.goobi.vocabularyserver.model.FieldInstanceEntity;
import io.goobi.vocabularyserver.model.VocabularyRecordEntity;
import org.springframework.stereotype.Service;

import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

@Service
public class RecordValidatorImpl extends BaseValidator<VocabularyRecordEntity> {
    private final Validator<FieldInstanceEntity> fieldValidator;

    public RecordValidatorImpl(Validator<FieldInstanceEntity> fieldValidator) {
        super("Record");
        this.fieldValidator = fieldValidator;
        setValidations(List.of(
                this::checkRequiredFieldsExistence,
                this::checkOnlyAllowedFieldDefinitionsGiven,
                this::checkHierarchy,
                this::perFieldInstanceChecks
        ));
    }

    private void checkRequiredFieldsExistence(VocabularyRecordEntity vocabularyRecord) throws RecordValidationException {
        List<FieldDefinitionEntity> missingFields = vocabularyRecord.getVocabulary()
                .getSchema()
                .getDefinitions()
                .stream()
                .filter(FieldDefinitionEntity::isRequired)
                .collect(Collectors.toList());
        List<FieldDefinitionEntity> providedFields = vocabularyRecord.getFields()
                .stream()
                .map(FieldInstanceEntity::getDefinition)
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

    private void checkOnlyAllowedFieldDefinitionsGiven(VocabularyRecordEntity vocabularyRecord) throws RecordValidationException {
        List<FieldDefinitionEntity> providedFields = vocabularyRecord.getFields()
                .stream()
                .map(FieldInstanceEntity::getDefinition)
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

    private void checkHierarchy(VocabularyRecordEntity vocabularyRecord) throws RecordValidationException {
        if (Boolean.FALSE.equals(vocabularyRecord.getVocabulary().getSchema().isHierarchicalRecords())) {
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

    private void perFieldInstanceChecks(VocabularyRecordEntity vocabularyRecord) throws RecordValidationException {
        List<Throwable> errors = new LinkedList<>();
        for (FieldInstanceEntity f : vocabularyRecord.getFields()) {
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
