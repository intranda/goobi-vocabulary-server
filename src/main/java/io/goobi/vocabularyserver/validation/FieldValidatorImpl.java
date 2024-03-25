package io.goobi.vocabularyserver.validation;

import io.goobi.vocabularyserver.exception.FieldValidationException;
import io.goobi.vocabularyserver.model.FieldInstance;
import org.springframework.stereotype.Service;

import java.util.regex.Pattern;

@Service
public class FieldValidatorImpl implements FieldValidator {
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
