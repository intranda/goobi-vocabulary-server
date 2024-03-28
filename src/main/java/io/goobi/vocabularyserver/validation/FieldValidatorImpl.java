package io.goobi.vocabularyserver.validation;

import io.goobi.vocabularyserver.exception.FieldValidationException;
import io.goobi.vocabularyserver.model.FieldInstance;
import io.goobi.vocabularyserver.model.FieldValue;
import io.goobi.vocabularyserver.model.Language;
import io.goobi.vocabularyserver.model.SelectableValue;
import io.goobi.vocabularyserver.repositories.FieldInstanceRepository;
import org.springframework.stereotype.Service;

import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

@Service
public class FieldValidatorImpl extends BaseValidator<FieldInstance> {

    private final FieldInstanceRepository fieldInstanceRepository;

    public FieldValidatorImpl(FieldInstanceRepository fieldInstanceRepository) {
        super("Field");
        setValidations(List.of(
//                this::checkRegularExpressionMatchesValue,
//                this::checkValueIsOneOfSelectableValues,
//                this::checkForbiddenBlankValue,
//                this::checkValueUniqueness
        ));
        this.fieldInstanceRepository = fieldInstanceRepository;
    }

//    private void checkRegularExpressionMatchesValue(FieldInstance field) throws FieldValidationException {
//        String regex = field.getDefinition().getType().getValidation();
//        List<String> wrongValues = null;
//        if (regex != null) {
//            for (FieldValue value : field.getFieldValues()) {
//                for (Map.Entry<Language, String> entry : value.getTranslations().entrySet()) {
//                    if (!Pattern.matches(regex, entry.getValue())) {
//                        if (wrongValues == null) {
//                            wrongValues = new LinkedList<>();
//                        }
//                        wrongValues.add("[" + entry.getKey().getName() + "] :: " + entry.getValue());
//                    }
//                }
//            }
//        }
//        if (wrongValues != null) {
//            throw new FieldValidationException("The following field values do not satisfy the validation \"" + regex + "\" for field \"" + field.getDefinition().getName() + "\" [" + field.getDefinition().getId() + "]:\n\t- "
//            + String.join("\n\t- ", wrongValues));
//        }
//    }

//    private void checkValueIsOneOfSelectableValues(FieldInstance field) throws FieldValidationException {
//        Set<SelectableValue> selectableValues = field.getDefinition().getType().getSelectableValues();
//        if (selectableValues != null && !selectableValues.isEmpty()) {
//            String value = field.getValue();
//            Set<String> selectableStringValues = selectableValues.stream().map(SelectableValue::getValue).collect(Collectors.toSet());
//            if (!selectableStringValues.contains(value)) {
//                throw new FieldValidationException("Field value \"" + value + "\" is not one of the allowed selectable values for field \""
//                        + field.getDefinition().getName() + "\" [" + field.getDefinition().getId() + "]: "
//                        + String.join(", ", selectableStringValues));
//            }
//        }
//    }
//
//    private void checkForbiddenBlankValue(FieldInstance field) throws FieldValidationException {
//        if (field.getValue().isBlank()) {
//            throw new FieldValidationException("Field \"" + field.getDefinition().getName() + "\" [" + field.getDefinition().getId() + "] value is not allowed to be blank");
//        }
//    }
//
//    private void checkValueUniqueness(FieldInstance field) throws FieldValidationException {
//        if (Boolean.TRUE.equals(field.getDefinition().getUnique())
//                && fieldInstanceRepository.existsByVocabularyRecord_Vocabulary_IdAndDefinition_IdAndValue(
//                        field.getVocabularyRecord().getVocabulary().getId(),
//                        field.getDefinition().getId(),
//                        field.getValue())) {
//            throw new FieldValidationException("Unique field value \"" + field.getValue() + "\" for field \"" +
//                    field.getDefinition().getName() + "\" [" + field.getDefinition().getId() + "] is already present in vocabulary");
//        }
//    }
}
