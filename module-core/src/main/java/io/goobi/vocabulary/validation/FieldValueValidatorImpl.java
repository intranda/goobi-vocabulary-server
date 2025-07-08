package io.goobi.vocabulary.validation;

import io.goobi.vocabulary.exception.EntityNotFoundException;
import io.goobi.vocabulary.exception.VocabularyException;
import io.goobi.vocabulary.model.jpa.*;
import io.goobi.vocabulary.repositories.FieldInstanceRepository;
import io.goobi.vocabulary.repositories.VocabularyRecordRepository;
import org.springframework.stereotype.Service;

import java.util.*;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

import static io.goobi.vocabulary.exception.VocabularyException.ErrorCode.*;

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
                this::checkTranslationsAreUnique,
                this::checkReferencedVocabularyRecordIsExistingInCorrectVocabulary
        ));
    }

    private void checkRegularExpressionMatchesValue(FieldValueEntity fieldValue) throws VocabularyException {
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
            throw new VocabularyException(FieldValuesDoNotMatchSpecifiedValidationRegex, null, Map.of(
                    "values", String.join(",", wrongValues),
                    "definitionId", String.valueOf(fieldValue.getFieldInstance().getDefinition().getId()),
                    "definitionName", fieldValue.getFieldInstance().getDefinition().getName(),
                    "regex", regex
            ),
                    params -> "The following field value(s) do not satisfy the validation \""
                            + params.get("regex") + "\" for field definition \"" + params.get("definitionName") + "\" ["
                            + params.get("definitionId") + "]: " + params.get("values"));
        }
    }

    private void checkValueIsOneOfSelectableValues(FieldValueEntity fieldValue) throws VocabularyException {
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
                throw new VocabularyException(FieldValuesAreNonSelectableValues, null, Map.of(
                        "values", String.join(",", wrongValues),
                        "selectableValues", String.join(",", selectableStringValues),
                        "definitionId", String.valueOf(fieldValue.getFieldInstance().getDefinition().getId()),
                        "definitionName", fieldValue.getFieldInstance().getDefinition().getName()
                ),
                        params -> "Field values(s) \"" + params.get("values") + "\" are not one of the allowed selectable values for field \""
                                + params.get("definitionName") + "\" [" + params.get("definitionId") + "]: " + params.get("selectableValues"));
            }
        }
    }

    private void checkForbiddenBlankValue(FieldValueEntity fieldValue) throws VocabularyException {
        if (fieldValue.getTranslations().stream().anyMatch(t -> t.getValue().isBlank())) {
            throw new VocabularyException(FieldValueIsBlank, null, Map.of(
                    "definitionId", String.valueOf(fieldValue.getFieldInstance().getDefinition().getId()),
                    "definitionName", fieldValue.getFieldInstance().getDefinition().getName()
            ),
                    params -> "Field definition \"" + params.get("definitionName") + "\" [" + params.get("definitionId") + "] value is not allowed to be blank");
        }
    }

    private void checkValueUniqueness(FieldValueEntity fieldValue) throws VocabularyException {
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
                throw new VocabularyException(FieldValueIsNotUnique, null, Map.of(
                        "definitionId", String.valueOf(fieldValue.getFieldInstance().getDefinition().getId()),
                        "definitionName", fieldValue.getFieldInstance().getDefinition().getName(),
                        "duplicateValues", String.join(",", duplicateUniqueValues)
                ),
                        params -> "Unique value(s) \"" + params.get("duplicateValues") + "\" for field \""
                                + params.get("definitionName") + " [" + params.get("definitionId") + "] is already present in vocabulary");
            }
        }
    }

    private void checkUntranslatableValueIsNotTranslated(FieldValueEntity fieldValue) throws VocabularyException {
        if (Boolean.TRUE.equals(fieldValue.getFieldInstance().getDefinition().getTranslationDefinitions().isEmpty())) {
            Set<String> nonDefinedTranslations = fieldValue.getTranslations().stream()
                    .filter(td -> td.getLanguage() != null)
                    .map(td -> td.getLanguage().getAbbreviation())
                    .collect(Collectors.toSet());
            if (!nonDefinedTranslations.isEmpty()) {
                throw new VocabularyException(FieldValueHasNonAllowedTranslations, null, Map.of(
                        "definitionId", String.valueOf(fieldValue.getFieldInstance().getDefinition().getId()),
                        "definitionName", fieldValue.getFieldInstance().getDefinition().getName(),
                        "translationLanguages", String.join(",", nonDefinedTranslations)
                ),
                        params -> "Non-translatable field \"" + params.get("definitionName") + "\" [" + params.get("definitionId")
                                + "] contains translations for the following languages: " + params.get("translationLanguages"));
            }
        }
    }

    private void checkTranslatableValueContainsNoNonTranslatedValues(FieldValueEntity fieldValue) throws VocabularyException {
        if (Boolean.FALSE.equals(fieldValue.getFieldInstance().getDefinition().getTranslationDefinitions().isEmpty())) {
            if (fieldValue.getTranslations().stream()
                    .anyMatch(t -> t.getLanguage() == null)) {
                throw new VocabularyException(FieldValueContainsNonTranslatedValue, null, Map.of(
                        "definitionId", String.valueOf(fieldValue.getFieldInstance().getDefinition().getId()),
                        "definitionName", fieldValue.getFieldInstance().getDefinition().getName()
                ),
                        params -> "Translatable field \"" + params.get("definitionName") + "\" [" + params.get("definitionId") + "] cannot contain non-translated values");
            }
        }
    }

    private void checkTranslatableValueHasNoUnspecifiedTranslations(FieldValueEntity fieldValue) throws VocabularyException {
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
                throw new VocabularyException(FieldValueUnspecifiedTranslations, null, Map.of(
                        "definitionId", String.valueOf(fieldValue.getFieldInstance().getDefinition().getId()),
                        "definitionName", fieldValue.getFieldInstance().getDefinition().getName(),
                        "unspecifiedTranslationLanguages", String.join(",", translationsGiven)
                ),
                        params -> "Translatable field \"" + params.get("definitionName") + "\" [" + params.get("definitionId")
                                + "] contains non-specified translations for the following languages: " + params.get("unspecifiedTranslationLanguages"));
            }
        }
    }

    private void checkTranslatableValueAllRequiredTranslationsProvided(FieldValueEntity fieldValue) throws VocabularyException {
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
                throw new VocabularyException(FieldValueMissingRequiredTranslations, null, Map.of(
                        "definitionId", String.valueOf(fieldValue.getFieldInstance().getDefinition().getId()),
                        "definitionName", fieldValue.getFieldInstance().getDefinition().getName(),
                        "requiredTranslationLanguages", String.join(",", specifiedRequiredLanguages)
                ),
                        params -> "Translatable field \"" + params.get("definitionName") + "\" [" + params.get("definitionId")
                                + "] is missing translations for the following required languages: " + params.get("requiredTranslationLanguages"));
            }
        }
    }

    private void checkTranslationsAreUnique(FieldValueEntity fieldValue) {
        if (fieldValue.getTranslations() == null || fieldValue.getTranslations().isEmpty()) {
            return;
        }
        List<String> allTranslationLanguages = fieldValue.getTranslations().stream()
                .map(FieldTranslationEntity::getLanguage)
                .filter(Objects::nonNull)
                .map(LanguageEntity::getAbbreviation)
                .toList();
        Set<String> uniqueTranslationLanguages = new HashSet<>(allTranslationLanguages);
        if (allTranslationLanguages.size() != uniqueTranslationLanguages.size()) {
            throw new VocabularyException(FieldValueMissingNonUniqueTranslations, null, Map.of(
                    "definitionId", String.valueOf(fieldValue.getFieldInstance().getDefinition().getId()),
                    "definitionName", fieldValue.getFieldInstance().getDefinition().getName(),
                    "givenTranslationLanguages", String.join(",", allTranslationLanguages)
            ),
                    params -> "Translatable field \"" + params.get("definitionName") + "\" [" + params.get("definitionId")
                            + "] has non unique translation languages given: " + params.get("givenTranslationLanguages"));
        }
    }

    private void checkReferencedVocabularyRecordIsExistingInCorrectVocabulary(FieldValueEntity fieldValue) throws VocabularyException {
        if (fieldValue.getFieldInstance().getDefinition().getReferenceVocabulary() != null) {
            long referenceVocabularyId = fieldValue.getFieldInstance().getDefinition().getReferenceVocabulary().getId();
            Set<Long> ids = fieldValue.getTranslations().stream()
                    .map(FieldTranslationEntity::getValue)
                    .map((Long::parseLong))
                    .collect(Collectors.toSet());
            List<VocabularyException> errors = new ArrayList<>(ids.size());
            ids.forEach(id -> {
                Optional<VocabularyRecordEntity> rec = recordRepository.findById(id);
                if (rec.isEmpty()) {
                    errors.add(new EntityNotFoundException(VocabularyRecordEntity.class, id));
                } else if (referenceVocabularyId != rec.get().getVocabulary().getId()) {
                    errors.add(new VocabularyException(FieldValueReferencedRecordBelongsToWrongVocabulary, null, Map.of(
                            "recordId", String.valueOf(id),
                            "referencedVocabulary", String.valueOf(rec.get().getVocabulary().getId()),
                            "specifiedReferenceVocabulary", String.valueOf(referenceVocabularyId)
                    ),
                            params -> "record [" + params.get("recordId") + "] belongs to vocabulary [" + params.get("referencedVocabulary")
                                    + "] instead of [" + params.get("specifiedReferenceVocabulary") + "]"));
                }
            });
            if (!errors.isEmpty()) {
                throw new VocabularyException(FieldValueReferencedRecordIssues, errors, null, params -> "Vocabulary record reference error(s)");
            }
        }
    }
}
