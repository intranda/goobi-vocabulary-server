package io.goobi.vocabulary.validation;

import io.goobi.vocabulary.exception.VocabularyException;
import io.goobi.vocabulary.model.jpa.FieldDefinitionEntity;
import io.goobi.vocabulary.model.jpa.FieldInstanceEntity;
import io.goobi.vocabulary.model.jpa.VocabularyRecordEntity;
import io.goobi.vocabulary.repositories.VocabularyRecordRepository;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import static io.goobi.vocabulary.exception.VocabularyException.ErrorCode.FieldInstanceIssues;
import static io.goobi.vocabulary.exception.VocabularyException.ErrorCode.RecordValidationChildrenReferencesNotAllowed;
import static io.goobi.vocabulary.exception.VocabularyException.ErrorCode.RecordValidationHierarchyIssues;
import static io.goobi.vocabulary.exception.VocabularyException.ErrorCode.RecordValidationMissingRequiredFields;
import static io.goobi.vocabulary.exception.VocabularyException.ErrorCode.RecordValidationParentReferenceNotAllowed;
import static io.goobi.vocabulary.exception.VocabularyException.ErrorCode.RecordValidationSingleRootElementVocabularyAlreadyContainsRecord;
import static io.goobi.vocabulary.exception.VocabularyException.ErrorCode.RecordValidationUndefinedFieldsGiven;

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

    private void checkRequiredFieldsExistence(VocabularyRecordEntity vocabularyRecord) throws VocabularyException {
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
            throw new VocabularyException(RecordValidationMissingRequiredFields, null, Map.of(
                    "missingFieldIds", missingFields.stream()
                            .map(FieldDefinitionEntity::getId)
                            .map(String::valueOf)
                            .collect(Collectors.joining(",")),
                    "missingFieldNames", missingFields.stream()
                            .map(FieldDefinitionEntity::getName)
                            .collect(Collectors.joining(",")),
                    "providedFieldIds", providedFields.stream()
                            .map(FieldDefinitionEntity::getId)
                            .map(String::valueOf)
                            .collect(Collectors.joining(",")),
                    "providedFieldNames", providedFields.stream()
                            .map(FieldDefinitionEntity::getName)
                            .collect(Collectors.joining(","))
            ),
                    params -> "Missing required fields: " + params.get("missingFieldNames")
            );
        }
    }

    private void checkOnlyAllowedFieldDefinitionsGiven(VocabularyRecordEntity vocabularyRecord) throws VocabularyException {
        List<FieldDefinitionEntity> providedFields = vocabularyRecord.getFields()
                .stream()
                .map(FieldInstanceEntity::getDefinition)
                .collect(Collectors.toList());
        providedFields.removeAll(vocabularyRecord.getSchema().getDefinitions());
        if (!providedFields.isEmpty()) {
            throw new VocabularyException(RecordValidationUndefinedFieldsGiven, null, Map.of(
                    "providedFieldIds", providedFields.stream()
                            .map(FieldDefinitionEntity::getId)
                            .map(String::valueOf)
                            .collect(Collectors.joining(",")),
                    "providedFieldNames", providedFields.stream()
                            .map(FieldDefinitionEntity::getName)
                            .collect(Collectors.joining(","))
            ),
                    params -> "Non-defined fields given: " + params.get("providedFieldIds")
            );
        }
    }

    private void checkHierarchy(VocabularyRecordEntity vocabularyRecord) throws VocabularyException {
        if (Boolean.FALSE.equals(vocabularyRecord.getSchema().isHierarchicalRecords())) {
            List<VocabularyException> errors = new ArrayList<>(2);
            if (vocabularyRecord.getParentRecord() != null) {
                errors.add(new VocabularyException(RecordValidationParentReferenceNotAllowed, null, Map.of(
                        "parentId", String.valueOf(vocabularyRecord.getParentRecord().getId())
                ),
                        params -> "Can't define parent record if hierarchical records are deactivated in vocabulary schema"));
            }
            if (!vocabularyRecord.getChildren().isEmpty()) {
                errors.add(new VocabularyException(RecordValidationChildrenReferencesNotAllowed, null, Map.of(
                        "vocabularyId", String.valueOf(vocabularyRecord.getVocabulary().getId()),
                        "vocabularyName", String.valueOf(vocabularyRecord.getVocabulary().getName()),
                        "childrenRecordIds", vocabularyRecord.getChildren().stream()
                                .map(VocabularyRecordEntity::getId)
                                .map(String::valueOf)
                                .collect(Collectors.joining(","))
                ),
                        params -> "Can't define children records if hierarchical records are deactivated in vocabulary schema"));
            }
            if (!errors.isEmpty()) {
                throw new VocabularyException(RecordValidationHierarchyIssues, errors, null,
                        params -> "Record violates hierarchy validation");
            }
        }
    }

    private void checkRootElementRestriction(VocabularyRecordEntity vocabularyRecord) throws VocabularyException {
        if (vocabularyRecord.getParentRecord() == null && Boolean.TRUE.equals(vocabularyRecord.getSchema().isSingleRootElement())) {
            if (vocabularyRecordRepository.existsByVocabulary_IdAndParentRecordNullAndIdNotAndMetadata(vocabularyRecord.getVocabulary().getId(), vocabularyRecord.getId(), vocabularyRecord.isMetadata())) {
                throw new VocabularyException(RecordValidationSingleRootElementVocabularyAlreadyContainsRecord, null, Map.of(
                        "vocabularyId", String.valueOf(vocabularyRecord.getVocabulary().getId()),
                        "vocabularyName", String.valueOf(vocabularyRecord.getVocabulary().getName())
                ),
                        params -> "Can't insert this root-level record, this vocabulary already contains a root-level record and is restricted to a single root-level record");
            }
        }
    }

    private void perFieldInstanceChecks(VocabularyRecordEntity vocabularyRecord) throws VocabularyException {
        List<VocabularyException> errors = new LinkedList<>();
        for (FieldInstanceEntity f : vocabularyRecord.getFields()) {
            try {
                fieldValidator.validate(f);
            } catch (VocabularyException e) {
                errors.add(e);
            }
        }
        if (!errors.isEmpty()) {
            throw new VocabularyException(FieldInstanceIssues, errors, Map.of(
                    "vocabularyId", String.valueOf(vocabularyRecord.getVocabulary().getId()),
                    "vocabularyName", String.valueOf(vocabularyRecord.getVocabulary().getName())
            ),
                    params -> "Error validating record fields");
        }
    }
}
