package io.goobi.vocabulary.validation;

import io.goobi.vocabulary.exception.FieldValueValidationException;
import io.goobi.vocabulary.model.jpa.FieldTranslationEntity;
import io.goobi.vocabulary.model.jpa.FieldValueEntity;
import io.goobi.vocabulary.model.jpa.SelectableValueEntity;
import io.goobi.vocabulary.model.jpa.TranslationDefinitionEntity;
import io.goobi.vocabulary.model.jpa.VocabularyRecordEntity;
import io.goobi.vocabulary.repositories.FieldInstanceRepository;
import io.goobi.vocabulary.repositories.VocabularyRecordRepository;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

@Service
public class FieldValueValidatorImpl extends BaseValidator<FieldValueEntity> {

    private final FieldInstanceRepository fieldInstanceRepository;
    private final VocabularyRecordRepository recordRepository;

    public FieldValueValidatorImpl(FieldInstanceRepository fieldInstanceRepository, VocabularyRecordRepository recordRepository) {
        super("Value");
        this.fieldInstanceRepository = fieldInstanceRepository;
        this.recordRepository = recordRepository;
        setValidations(List.of(
                this::checkRegularExpressionMatchesValue,
                this::checkValueIsOneOfSelectableValues,
                this::checkForbiddenBlankValue,
                this::checkValueUniqueness,
                this::checkUntranslatableValueIsNotTranslated,
                this::checkTranslatableValueContainsNoNonTranslatedValues,
                this::checkTranslatableValueHasNoUnspecifiedTranslations,
                this::checkTranslatableValueAllRequiredTranslationsProvided,
                this::checkReferencedVocabularyRecordIsExistingInCorrectVocabulary
        ));
    }

    private void checkRegularExpressionMatchesValue(FieldValueEntity fieldValue) throws FieldValueValidationException {
        if (fieldValue.getFieldInstance().getDefinition().getType() == null) {
            return;
        }
        String regex = fieldValue.getFieldInstance().getDefinition().getType().getValidation();
        List<String> wrongValues = null;
        if (regex != null) {
            for (FieldTranslationEntity translation : fieldValue.getTranslations()) {
                if (!Pattern.matches(regex, translation.getValue())) {
                    if (wrongValues == null) {
                        wrongValues = new LinkedList<>();
                    }
                    String lang = translation.getLanguage() != null ? "[" + translation.getLanguage().getName() + "] :: " : "";
                    wrongValues.add(lang + translation.getValue());
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
        if (fieldValue.getFieldInstance().getDefinition().getType() == null) {
            return;
        }
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
                    .filter(v -> fieldInstanceRepository.existsByVocabularyRecord_Vocabulary_IdAndDefinition_IdAndVocabularyRecord_IdNotAndFieldValues_Translations_Value(
                            fieldValue.getFieldInstance().getVocabularyRecord().getVocabulary().getId(),
                            fieldValue.getFieldInstance().getDefinition().getId(),
                            fieldValue.getFieldInstance().getVocabularyRecord().getId(),
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

    private void checkUntranslatableValueIsNotTranslated(FieldValueEntity fieldValue) throws FieldValueValidationException {
        if (Boolean.TRUE.equals(fieldValue.getFieldInstance().getDefinition().getTranslationDefinitions().isEmpty())) {
            Set<String> nonDefinedTranslations = fieldValue.getTranslations().stream()
                    .filter(td -> td.getLanguage() != null)
                    .map(td -> td.getLanguage().getAbbreviation())
                    .collect(Collectors.toSet());
            if (!nonDefinedTranslations.isEmpty()) {
                throw new FieldValueValidationException("Non-translatable field \"" + fieldValue.getFieldInstance().getDefinition().getName()
                        + "\" [" + fieldValue.getFieldInstance().getDefinition().getId() + "] contains translations for the following languages: " + String.join(", ", nonDefinedTranslations));
            }
        }
    }

    private void checkTranslatableValueContainsNoNonTranslatedValues(FieldValueEntity fieldValue) throws FieldValueValidationException {
        if (Boolean.FALSE.equals(fieldValue.getFieldInstance().getDefinition().getTranslationDefinitions().isEmpty())) {
            if (fieldValue.getTranslations().stream()
                    .anyMatch(t -> t.getLanguage() == null)) {
                throw new FieldValueValidationException("Translatable field \"" + fieldValue.getFieldInstance().getDefinition().getName()
                        + "\" [" + fieldValue.getFieldInstance().getDefinition().getId() + "] cannot contain non-translated values");
            }
        }
    }

    private void checkTranslatableValueHasNoUnspecifiedTranslations(FieldValueEntity fieldValue) throws FieldValueValidationException {
        if (Boolean.FALSE.equals(fieldValue.getFieldInstance().getDefinition().getTranslationDefinitions().isEmpty())) {
            Set<String> specifiedLanguages = fieldValue.getFieldInstance().getDefinition().getTranslationDefinitions().stream()
                    .map(td -> td.getLanguage().getAbbreviation())
                    .collect(Collectors.toSet());
            Set<String> translationsGiven = fieldValue.getTranslations().stream()
                    .filter(t -> t.getLanguage() != null)
                    .map(t -> t.getLanguage().getAbbreviation())
                    .collect(Collectors.toSet());
            translationsGiven.removeAll(specifiedLanguages);
            if (!translationsGiven.isEmpty()) {
                throw new FieldValueValidationException("Translatable field \"" + fieldValue.getFieldInstance().getDefinition().getName()
                        + "\" [" + fieldValue.getFieldInstance().getDefinition().getId() + "] contains non-specified translations for the following languages: " + String.join(", ", translationsGiven));
            }
        }
    }

    private void checkTranslatableValueAllRequiredTranslationsProvided(FieldValueEntity fieldValue) throws FieldValueValidationException {
        if (Boolean.FALSE.equals(fieldValue.getFieldInstance().getDefinition().getTranslationDefinitions().isEmpty())) {
            Set<String> specifiedRequiredLanguages = fieldValue.getFieldInstance().getDefinition().getTranslationDefinitions().stream()
                    .filter(TranslationDefinitionEntity::isRequired)
                    .map(td -> td.getLanguage().getAbbreviation())
                    .collect(Collectors.toSet());
            Set<String> translationsGiven = fieldValue.getTranslations().stream()
                    .filter(t -> t.getLanguage() != null)
                    .map(t -> t.getLanguage().getAbbreviation())
                    .collect(Collectors.toSet());
            specifiedRequiredLanguages.removeAll(translationsGiven);
            if (!specifiedRequiredLanguages.isEmpty()) {
                throw new FieldValueValidationException("Translatable field \"" + fieldValue.getFieldInstance().getDefinition().getName()
                        + "\" [" + fieldValue.getFieldInstance().getDefinition().getId() + "] is missing translations for the following required languages: " + String.join(", ", specifiedRequiredLanguages));
            }
        }
    }

    private void checkReferencedVocabularyRecordIsExistingInCorrectVocabulary(FieldValueEntity fieldValue) throws FieldValueValidationException {
        if (fieldValue.getFieldInstance().getDefinition().getReferenceVocabulary() != null) {
            long referenceVocabularyId = fieldValue.getFieldInstance().getDefinition().getReferenceVocabulary().getId();
            Set<Long> ids = fieldValue.getTranslations().stream()
                    .map(FieldTranslationEntity::getValue)
                    .map((Long::parseLong))
                    .collect(Collectors.toSet());
            List<String> errors = new ArrayList<>(ids.size());
            ids.forEach(id -> {
                Optional<VocabularyRecordEntity> rec = recordRepository.findById(id);
                if (rec.isEmpty()) {
                    errors.add("record [" + id + "] does not exist");
                } else if (referenceVocabularyId != rec.get().getVocabulary().getId()) {
                    errors.add("record [" + id + "] belongs to vocabulary [" + rec.get().getVocabulary().getId() + "] instead of [" + referenceVocabularyId + "]");
                }
            });
            if (!errors.isEmpty()) {
                throw new FieldValueValidationException("Vocabulary record reference error(s): " + String.join(", ", errors));
            }
        }
    }
}
