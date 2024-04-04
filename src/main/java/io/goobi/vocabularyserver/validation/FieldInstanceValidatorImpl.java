package io.goobi.vocabularyserver.validation;

import io.goobi.vocabularyserver.exception.FieldInstanceValidationException;
import io.goobi.vocabularyserver.exception.ValidationException;
import io.goobi.vocabularyserver.model.FieldInstance;
import io.goobi.vocabularyserver.model.FieldValue;
import io.goobi.vocabularyserver.repositories.FieldInstanceRepository;
import org.springframework.stereotype.Service;

import java.util.LinkedList;
import java.util.List;
import java.util.stream.Collectors;

@Service
public class FieldInstanceValidatorImpl extends BaseValidator<FieldInstance> {
    private final Validator<FieldValue> fieldValueValidator;

    public FieldInstanceValidatorImpl(FieldInstanceRepository fieldInstanceRepository) {
        super("Field");
        this.fieldValueValidator = new FieldValueValidatorImpl(fieldInstanceRepository);
        setValidations(List.of(
                this::perValueChecks,
                this::multiValueCheck
        ));
    }

    private void perValueChecks(FieldInstance fieldInstance) throws FieldInstanceValidationException {
        List<Throwable> errors = new LinkedList<>();
        for (FieldValue fv : fieldInstance.getFieldValues()) {
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

    private void multiValueCheck(FieldInstance fieldInstance) throws FieldInstanceValidationException {
        if (Boolean.FALSE.equals(fieldInstance.getDefinition().getMultiValued()) && fieldInstance.getFieldValues().size() > 1) {
            throw new FieldInstanceValidationException("The field definition \"" + fieldInstance.getDefinition().getName() + "\" ["
                    + fieldInstance.getDefinition().getId() + "] is not multi-valued, but " + fieldInstance.getFieldValues().size()
                    + " values were provided");
        }
    }
}
