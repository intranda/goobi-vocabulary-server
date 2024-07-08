package io.goobi.vocabulary.validation;

import io.goobi.vocabulary.exception.RecordValidationException;
import io.goobi.vocabulary.exception.ValidationException;
import io.goobi.vocabulary.model.jpa.FieldDefinitionEntity;
import io.goobi.vocabulary.model.jpa.FieldInstanceEntity;
import io.goobi.vocabulary.model.jpa.VocabularyRecordEntity;
import io.goobi.vocabulary.repositories.VocabularyRecordRepository;
import org.springframework.stereotype.Service;

import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

@Service
public class RecordValidatorImpl extends BaseValidator<VocabularyRecordEntity> {
    private final Validator<FieldInstanceEntity> fieldValidator;
    private final VocabularyRecordRepository vocabularyRecordRepository;

    public RecordValidatorImpl(Validator<FieldInstanceEntity> fieldValidator, VocabularyRecordRepository vocabularyRecordRepository) {
        super("Record");
        this.fieldValidator = fieldValidator;
        setValidations(List.of(
                this::checkRequiredFieldsExistence,
                this::checkOnlyAllowedFieldDefinitionsGiven,
                this::checkHierarchy,
                this::checkRootElementRestriction,
                this::perFieldInstanceChecks
        ));
        this.vocabularyRecordRepository = vocabularyRecordRepository;
    }

    private void checkRequiredFieldsExistence(VocabularyRecordEntity vocabularyRecord) throws RecordValidationException {
        List<FieldDefinitionEntity> missingFields = vocabularyRecord.getSchema()
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
        providedFields.removeAll(vocabularyRecord.getSchema().getDefinitions());
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
        if (Boolean.FALSE.equals(vocabularyRecord.getSchema().isHierarchicalRecords())) {
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

    private void checkRootElementRestriction(VocabularyRecordEntity vocabularyRecord) throws RecordValidationException {
        if (vocabularyRecord.getParentRecord() == null && Boolean.TRUE.equals(vocabularyRecord.getSchema().isSingleRootElement())) {
            if (vocabularyRecordRepository.existsByVocabulary_IdAndParentRecordNullAndIdNotAndMetadata(vocabularyRecord.getVocabulary().getId(), vocabularyRecord.getId(), vocabularyRecord.isMetadata())) {
                throw new RecordValidationException("Can't insert this root-level record, this vocabulary already contains a root-level record and is restricted to a single root-level record");
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
