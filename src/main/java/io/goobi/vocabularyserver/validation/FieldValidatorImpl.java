package io.goobi.vocabularyserver.validation;

import io.goobi.vocabularyserver.exception.FieldValidationException;
import io.goobi.vocabularyserver.model.FieldInstance;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.regex.Pattern;

@Service
public class FieldValidatorImpl implements Validator<FieldInstance> {
    private final List<ValidationMethod<FieldInstance>> validations;

    public FieldValidatorImpl() {
        validations = List.of(this::validateValue);
    }

    @Override
    public void validate(FieldInstance field) throws FieldValidationException {
        validateValue(field);

    }

    private void validateValue(FieldInstance field) throws FieldValidationException {
        String value = field.getValue();
        String regex = field.getDefinition().getType().getValidation();
        if (regex != null && !Pattern.matches(regex, value)) {
            throw new FieldValidationException("Field value \"" + value + "\" doesn't satisfy validation regular expression \"" + regex + "\"");
        }
    }
}
