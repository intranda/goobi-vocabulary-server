package io.goobi.vocabularyserver.validation;

import io.goobi.vocabularyserver.exception.FieldValidationException;
import io.goobi.vocabularyserver.model.FieldInstance;
import io.goobi.vocabularyserver.model.SelectableValue;
import io.goobi.vocabularyserver.repositories.FieldInstanceRepository;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Set;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

@Service
public class FieldValidatorImpl extends BaseValidator<FieldInstance> {

    private final FieldInstanceRepository fieldInstanceRepository;

    public FieldValidatorImpl(FieldInstanceRepository fieldInstanceRepository) {
        super("Field");
        setValidations(List.of(
                this::checkRegularExpressionMatchesValue,
                this::checkValueIsOneOfSelectableValues,
                this::checkForbiddenBlankValue,
                this::checkValueUniqueness
        ));
        this.fieldInstanceRepository = fieldInstanceRepository;
    }

    private void checkRegularExpressionMatchesValue(FieldInstance field) throws FieldValidationException {
        String value = field.getValue();
        String regex = field.getDefinition().getType().getValidation();
        if (regex != null && !Pattern.matches(regex, value)) {
            throw new FieldValidationException("Field value \"" + value + "\" doesn't satisfy validation regular expression \"" + regex + "\"");
        }
    }

    private void checkValueIsOneOfSelectableValues(FieldInstance fieldInstance) throws FieldValidationException {
        Set<SelectableValue> selectableValues = fieldInstance.getDefinition().getType().getSelectableValues();
        if (selectableValues != null && !selectableValues.isEmpty()) {
            String value = fieldInstance.getValue();
            Set<String> selectableStringValues = selectableValues.stream().map(SelectableValue::getValue).collect(Collectors.toSet());
            if (!selectableStringValues.contains(value)) {
                throw new FieldValidationException("Field value \"" + value + "\" is not one of the allowed selectable values: " + String.join(", ", selectableStringValues));
            }
        }
    }

    private void checkForbiddenBlankValue(FieldInstance field) throws FieldValidationException {
        if (field.getValue().isBlank()) {
            throw new FieldValidationException("Field value is not allowed to be blank");
        }
    }

    private void checkValueUniqueness(FieldInstance field) throws FieldValidationException {
        if (field.getDefinition().getUnique() && fieldInstanceRepository.existsByVocabularyRecord_Vocabulary_IdAndValue(field.getVocabularyRecord().getVocabulary().getId(), field.getValue())) {
            throw new FieldValidationException("Unique field value \"" + field.getValue() + "\" is already present in vocabulary");
        }
    }
}
