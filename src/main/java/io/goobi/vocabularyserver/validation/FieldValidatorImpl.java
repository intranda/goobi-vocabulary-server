package io.goobi.vocabularyserver.validation;

import io.goobi.vocabularyserver.exception.FieldValidationException;
import io.goobi.vocabularyserver.model.FieldInstance;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.regex.Pattern;

@Service
public class FieldValidatorImpl extends BaseValidator<FieldInstance> {

    public FieldValidatorImpl() {
        super("Field Instance");
        setValidations(List.of(
                this::validateValueRegularExpressionMatch,
                this::checkForbiddenBlankValue
        ));
    }

    private void validateValueRegularExpressionMatch(FieldInstance field) throws FieldValidationException {
        String value = field.getValue();
        String regex = field.getDefinition().getType().getValidation();
        if (regex != null && !Pattern.matches(regex, value)) {
            throw new FieldValidationException("Field value \"" + value + "\" doesn't satisfy validation regular expression \"" + regex + "\"");
        }
    }

    private void checkForbiddenBlankValue(FieldInstance field) throws FieldValidationException {
        if (field.getValue().isBlank()) {
            throw new FieldValidationException("Field value is not allowed to be blank");
        }
    }
}
