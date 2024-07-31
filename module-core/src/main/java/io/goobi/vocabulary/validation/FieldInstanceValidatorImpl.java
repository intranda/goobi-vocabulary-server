package io.goobi.vocabulary.validation;

import io.goobi.vocabulary.exception.VocabularyException;
import io.goobi.vocabulary.model.jpa.FieldInstanceEntity;
import io.goobi.vocabulary.model.jpa.FieldValueEntity;
import io.goobi.vocabulary.repositories.FieldInstanceRepository;
import io.goobi.vocabulary.repositories.VocabularyRecordRepository;
import org.springframework.stereotype.Service;

import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import static io.goobi.vocabulary.exception.VocabularyException.ErrorCode.FieldInstanceBelongsToWrongSchema;
import static io.goobi.vocabulary.exception.VocabularyException.ErrorCode.FieldInstanceIsEmpty;
import static io.goobi.vocabulary.exception.VocabularyException.ErrorCode.FieldInstanceMultipleValuesNotAllowed;
import static io.goobi.vocabulary.exception.VocabularyException.ErrorCode.FieldInstanceValueIssues;

@Service
public class FieldInstanceValidatorImpl extends BaseValidator<FieldInstanceEntity> {
    private final Validator<FieldValueEntity> fieldValueValidator;

    public FieldInstanceValidatorImpl(FieldInstanceRepository fieldInstanceRepository, VocabularyRecordRepository vocabularyRecordRepository) {
        super("Field");
        this.fieldValueValidator = new FieldValueValidatorImpl(fieldInstanceRepository, vocabularyRecordRepository);
        setValidations(List.of(
                this::fieldIsNotEmptyCheck,
                this::fieldDefinitionBelongsToCorrectSchema,
                this::perValueChecks,
                this::multiValueCheck
        ));
    }

    private void fieldIsNotEmptyCheck(FieldInstanceEntity fieldInstance) throws VocabularyException {
        if (fieldInstance.getFieldValues().isEmpty()) {
            throw new VocabularyException(FieldInstanceIsEmpty, null, null, params -> "The field is empty");
        }
    }

    private void fieldDefinitionBelongsToCorrectSchema(FieldInstanceEntity fieldInstance) throws VocabularyException {
        long instanceSchema = fieldInstance.getDefinition().getSchema().getId();
        long correctSchema = fieldInstance.getVocabularyRecord().getSchema().getId();
        if (instanceSchema != correctSchema) {
            throw new VocabularyException(FieldInstanceBelongsToWrongSchema, null, Map.of(
                    "definitionId", String.valueOf(fieldInstance.getDefinition().getId()),
                    "definitionName", fieldInstance.getDefinition().getName(),
                    "specifiedSchemaId", String.valueOf(instanceSchema),
                    "requiredSchemaId", String.valueOf(correctSchema)
            ),
                    params -> "The field definition \"" + params.get("definitionName") + "\" [" + params.get("definitionId") + "] belongs to schema ["
                            + params.get("specifiedSchemaId") + "], but this vocabulary uses schema [" + params.get("requiredSchemaId") + "]");
        }
    }

    private void perValueChecks(FieldInstanceEntity fieldInstance) throws VocabularyException {
        List<VocabularyException> errors = new LinkedList<>();
        for (FieldValueEntity fv : fieldInstance.getFieldValues()) {
            try {
                fieldValueValidator.validate(fv);
            } catch (VocabularyException e) {
                errors.add(e);
            }
        }
        if (!errors.isEmpty()) {
            String errorMessages = errors.stream().map(Throwable::getMessage).collect(Collectors.joining("\n\t"));
            throw new VocabularyException(FieldInstanceValueIssues, errors, null, params -> "Error validating field value, reason(s): \n\t" + errorMessages);
        }
    }

    private void multiValueCheck(FieldInstanceEntity fieldInstance) throws VocabularyException {
        if (Boolean.FALSE.equals(fieldInstance.getDefinition().isMultiValued()) && fieldInstance.getFieldValues().size() > 1) {
            throw new VocabularyException(FieldInstanceMultipleValuesNotAllowed, null, Map.of(
                    "definitionId", String.valueOf(fieldInstance.getDefinition().getId()),
                    "definitionName", fieldInstance.getDefinition().getName(),
                    "numberOfValues", String.valueOf(fieldInstance.getFieldValues().size())
            ), params -> "The field definition \"" + params.get("definitionName") + "\" [" + params.get("definitionId") + "] is not multi-valued, but " + params.get("numberOfValues") + " values were provided");
        }
    }
}
