package io.goobi.vocabulary.validation;

import io.goobi.vocabulary.exception.FieldInstanceValidationException;
import io.goobi.vocabulary.exception.ValidationException;
import io.goobi.vocabulary.model.jpa.FieldInstanceEntity;
import io.goobi.vocabulary.model.jpa.FieldValueEntity;
import io.goobi.vocabulary.repositories.FieldInstanceRepository;
import org.springframework.stereotype.Service;

import java.util.LinkedList;
import java.util.List;
import java.util.stream.Collectors;

@Service
public class FieldInstanceValidatorImpl extends BaseValidator<FieldInstanceEntity> {
    private final Validator<FieldValueEntity> fieldValueValidator;

    public FieldInstanceValidatorImpl(FieldInstanceRepository fieldInstanceRepository) {
        super("Field");
        this.fieldValueValidator = new FieldValueValidatorImpl(fieldInstanceRepository);
        setValidations(List.of(
                this::fieldIsNotEmptyCheck,
                this::fieldDefinitionBelongsToCorrectSchema,
                this::perValueChecks,
                this::multiValueCheck
        ));
    }

    private void fieldIsNotEmptyCheck(FieldInstanceEntity fieldInstance) throws FieldInstanceValidationException {
        if (fieldInstance.getFieldValues().isEmpty()) {
            throw new FieldInstanceValidationException("The field is empty");
        }
    }

    private void fieldDefinitionBelongsToCorrectSchema(FieldInstanceEntity fieldInstance) throws FieldInstanceValidationException {
        long instanceSchema = fieldInstance.getDefinition().getSchema().getId();
        long correctSchema = fieldInstance.getVocabularyRecord().getVocabulary().getSchema().getId();
        if (instanceSchema != correctSchema) {
            throw new FieldInstanceValidationException("The field definition \"" + fieldInstance.getDefinition().getName() + "\" ["
                    + fieldInstance.getDefinition().getId() + "] belongs to schema [" + instanceSchema + "], but this vocabulary uses schema [" + correctSchema + "]");
        }
    }

    private void perValueChecks(FieldInstanceEntity fieldInstance) throws FieldInstanceValidationException {
        List<Throwable> errors = new LinkedList<>();
        for (FieldValueEntity fv : fieldInstance.getFieldValues()) {
            try {
                fieldValueValidator.validate(fv);
            } catch (ValidationException e) {
                errors.add(e);
            }
        }
        if (!errors.isEmpty()) {
            String errorMessages = errors.stream().map(Throwable::getMessage).collect(Collectors.joining("\n"));
            throw new FieldInstanceValidationException(errorMessages);
        }
    }

    private void multiValueCheck(FieldInstanceEntity fieldInstance) throws FieldInstanceValidationException {
        if (Boolean.FALSE.equals(fieldInstance.getDefinition().isMultiValued()) && fieldInstance.getFieldValues().size() > 1) {
            throw new FieldInstanceValidationException("The field definition \"" + fieldInstance.getDefinition().getName() + "\" ["
                    + fieldInstance.getDefinition().getId() + "] is not multi-valued, but " + fieldInstance.getFieldValues().size()
                    + " values were provided");
        }
    }
}
