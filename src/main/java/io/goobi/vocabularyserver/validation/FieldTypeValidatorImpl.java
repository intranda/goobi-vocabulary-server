package io.goobi.vocabularyserver.validation;

import io.goobi.vocabularyserver.exception.FieldTypeValidationException;
import io.goobi.vocabularyserver.model.FieldType;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.regex.Pattern;
import java.util.regex.PatternSyntaxException;

@Service
public class FieldTypeValidatorImpl extends BaseValidator<FieldType> {
    public FieldTypeValidatorImpl() {
        super("Schema");
        setValidations(List.of(
                this::checkValidRegex
        ));
    }

    private void checkValidRegex(FieldType fieldType) throws FieldTypeValidationException {
        if (fieldType.getValidation() != null && !fieldType.getValidation().isEmpty()) {
            try {
                Pattern.compile(fieldType.getValidation());
            } catch (PatternSyntaxException e) {
                throw new FieldTypeValidationException("FieldType validation \"" + fieldType.getValidation() + "\" is no valid regex expression");
            }
        }
    }
}
