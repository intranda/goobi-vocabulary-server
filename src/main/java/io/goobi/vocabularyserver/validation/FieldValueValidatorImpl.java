package io.goobi.vocabularyserver.validation;

import io.goobi.vocabularyserver.exception.FieldValueValidationException;
import io.goobi.vocabularyserver.model.FieldTranslationEntity;
import io.goobi.vocabularyserver.model.FieldValueEntity;
import io.goobi.vocabularyserver.model.SelectableValueEntity;
import io.goobi.vocabularyserver.repositories.FieldInstanceRepository;
import org.springframework.stereotype.Service;

import java.util.LinkedList;
import java.util.List;
import java.util.Set;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

@Service
public class FieldValueValidatorImpl extends BaseValidator<FieldValueEntity> {

    private final FieldInstanceRepository fieldInstanceRepository;

    public FieldValueValidatorImpl(FieldInstanceRepository fieldInstanceRepository) {
        super("Value");
        setValidations(List.of(
                this::checkRegularExpressionMatchesValue,
                this::checkValueIsOneOfSelectableValues,
                this::checkForbiddenBlankValue,
                this::checkValueUniqueness
        ));
        this.fieldInstanceRepository = fieldInstanceRepository;
    }

    private void checkRegularExpressionMatchesValue(FieldValueEntity fieldValue) throws FieldValueValidationException {
        String regex = fieldValue.getFieldInstance().getDefinition().getType().getValidation();
        List<String> wrongValues = null;
        if (regex != null) {
            for (FieldTranslationEntity translation : fieldValue.getTranslations()) {
                if (!Pattern.matches(regex, translation.getValue())) {
                    if (wrongValues == null) {
                        wrongValues = new LinkedList<>();
                    }
                    wrongValues.add("[" + translation.getLanguage().getName() + "] :: " + translation.getValue());
                }
            }
        }
        if (wrongValues != null) {
            throw new FieldValueValidationException("The following field translation(s) do not satisfy the validation \""
                    + regex + "\" for field \"" + fieldValue.getFieldInstance().getDefinition().getName() + "\" ["
                    + fieldValue.getFieldInstance().getDefinition().getId() + "]:\n\t- "
                    + String.join("\n\t- ", wrongValues));
        }
    }

    private void checkValueIsOneOfSelectableValues(FieldValueEntity fieldValue) throws FieldValueValidationException {
        List<SelectableValueEntity> selectableValues = fieldValue.getFieldInstance().getDefinition().getType().getSelectableValues();
        if (selectableValues != null && !selectableValues.isEmpty()) {
            Set<String> selectableStringValues = selectableValues.stream()
                    .map(SelectableValueEntity::getValue)
                    .collect(Collectors.toSet());
            Set<String> wrongValues = fieldValue.getTranslations().stream()
                    .map(FieldTranslationEntity::getValue)
                    .filter(v -> !selectableStringValues.contains(v))
                    .collect(Collectors.toSet());
            if (!wrongValues.isEmpty()) {
                throw new FieldValueValidationException("Field translation(s) \"" + String.join("\", \"", wrongValues) + "\" is not one of the allowed selectable values for field \""
                        + fieldValue.getFieldInstance().getDefinition().getName() + "\" [" + fieldValue.getFieldInstance().getDefinition().getId() + "]: "
                        + String.join(", ", selectableStringValues));
            }
        }
    }

    private void checkForbiddenBlankValue(FieldValueEntity fieldValue) throws FieldValueValidationException {
        if (fieldValue.getTranslations().stream().anyMatch(t -> t.getValue().isBlank())) {
            throw new FieldValueValidationException("Field definition \"" + fieldValue.getFieldInstance().getDefinition().getName()
                    + "\" [" + fieldValue.getFieldInstance().getDefinition().getId() + "] value is not allowed to be blank");
        }
    }

    private void checkValueUniqueness(FieldValueEntity fieldValue) throws FieldValueValidationException {
        if (Boolean.TRUE.equals(fieldValue.getFieldInstance().getDefinition().isUnique())) {
            Set<String> duplicateUniqueValues = fieldValue.getTranslations().stream()
                    .map(FieldTranslationEntity::getValue)
                    .filter(v -> fieldInstanceRepository.existsByVocabularyRecord_Vocabulary_IdAndDefinition_IdAndIdNotAndFieldValues_Translations_Value(
                                fieldValue.getFieldInstance().getVocabularyRecord().getVocabulary().getId(),
                                fieldValue.getFieldInstance().getDefinition().getId(),
                                fieldValue.getFieldInstance().getId(),
                                v
                    ))
                    .collect(Collectors.toSet());
            if (!duplicateUniqueValues.isEmpty()) {
                throw new FieldValueValidationException("Unique value(s) \"" + String.join("\", \"", duplicateUniqueValues)
                        + "\" for field \"" + fieldValue.getFieldInstance().getDefinition().getName() + "\" ["
                        + fieldValue.getFieldInstance().getDefinition().getId() + "] is already present in vocabulary");
            }
        }
    }
}
