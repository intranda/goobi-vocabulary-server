package io.goobi.vocabulary.validation;

import io.goobi.vocabulary.exception.VocabularyException;
import io.goobi.vocabulary.model.jpa.FieldTypeEntity;
import io.goobi.vocabulary.model.jpa.SelectableValueEntity;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.regex.Pattern;
import java.util.regex.PatternSyntaxException;
import java.util.stream.Collectors;

import static io.goobi.vocabulary.exception.VocabularyException.ErrorCode.FieldTypeInvalidRegexSyntax;
import static io.goobi.vocabulary.exception.VocabularyException.ErrorCode.FieldTypeSelectableValuesDoNotMatchSpecifiedValidationRegex;

@Service
public class FieldTypeValidatorImpl extends BaseValidator<FieldTypeEntity> {
    public FieldTypeValidatorImpl() {
        super("Type");
        setValidations(List.of(
                this::checkValidRegex,
                this::checkSelectableValuesMatchValidation
        ));
    }

    private void checkValidRegex(FieldTypeEntity fieldType) throws VocabularyException {
        if (fieldType.getValidation() != null && !fieldType.getValidation().isEmpty()) {
            try {
                Pattern.compile(fieldType.getValidation());
            } catch (PatternSyntaxException e) {
                throw new VocabularyException(FieldTypeInvalidRegexSyntax, null, Map.of(
                        "regex", fieldType.getValidation()
                ),
                        params -> "Validation \"" + params.get("regex") + "\" is no valid regex expression");
            }
        }
    }

    private void checkSelectableValuesMatchValidation(FieldTypeEntity fieldType) throws VocabularyException {
        if (!fieldType.getSelectableValues().isEmpty() && fieldType.getValidation() != null && !fieldType.getValidation().isEmpty()) {
            Set<String> errorValues = fieldType.getSelectableValues().stream()
                    .map(SelectableValueEntity::getValue)
                    .filter(v -> !Pattern.matches(fieldType.getValidation(), v))
                    .collect(Collectors.toSet());
            if (!errorValues.isEmpty()) {
                throw new VocabularyException(FieldTypeSelectableValuesDoNotMatchSpecifiedValidationRegex, null, Map.of(
                        "selectableValues", String.join(",", errorValues),
                        "regex", fieldType.getValidation()
                ),
                        params -> "Selectable value(s) do not satisfy validation \"" + params.get("regex") + "\": \"" + params.get("selectableValues") + "\"");
            }
        }
    }
}
