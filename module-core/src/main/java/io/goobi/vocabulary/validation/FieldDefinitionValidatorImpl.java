package io.goobi.vocabulary.validation;

import io.goobi.vocabulary.exception.VocabularyException;
import io.goobi.vocabulary.model.jpa.FieldDefinitionEntity;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import static io.goobi.vocabulary.exception.VocabularyException.ErrorCode.FieldDefinitionFallbackNotRequired;
import static io.goobi.vocabulary.exception.VocabularyException.ErrorCode.FieldDefinitionNeitherReferenceVocabularyNorTypeNotPossible;
import static io.goobi.vocabulary.exception.VocabularyException.ErrorCode.FieldDefinitionNonUniqueFallbackLanguageSpecified;
import static io.goobi.vocabulary.exception.VocabularyException.ErrorCode.FieldDefinitionReferenceVocabularyAndTypeNotPossible;

@Service
public class FieldDefinitionValidatorImpl extends BaseValidator<FieldDefinitionEntity> {
    public FieldDefinitionValidatorImpl() {
        super("Definition");
        setValidations(List.of(
                this::ifTranslatedExactlyOneFallbackLanguageSpecified,
                this::fallbackLanguageMustBeRequired,
                this::eitherVocabularyReferenceOrTypeRequiredWithOptionalTranslationDefinitions
        ));
    }

    private void ifTranslatedExactlyOneFallbackLanguageSpecified(FieldDefinitionEntity fieldDefinition) throws VocabularyException {
        if (!fieldDefinition.getTranslationDefinitions().isEmpty()) {
            long fallbacksSpecified = fieldDefinition.getTranslationDefinitions().stream()
                    .filter(t -> Boolean.TRUE.equals(t.getFallback()))
                    .count();
            if (fallbacksSpecified != 1) {
                throw new VocabularyException(FieldDefinitionNonUniqueFallbackLanguageSpecified, null, Map.of(
                        "name", fieldDefinition.getName(),
                        "numberOfSpecifiedFallbackLanguages", String.valueOf(fallbacksSpecified)
                ),
                        params -> "Exactly one fallback language is required, but " + params.get("numberOfSpecifiedFallbackLanguages") + " were specified");
            }
        }
    }

    private void fallbackLanguageMustBeRequired(FieldDefinitionEntity fieldDefinition) throws VocabularyException {
        if (fieldDefinition.getTranslationDefinitions().stream()
                .filter(t -> Boolean.TRUE.equals(t.getFallback()))
                .anyMatch(t -> !t.isRequired())) {
            throw new VocabularyException(FieldDefinitionFallbackNotRequired, null, Map.of(
                    "name", fieldDefinition.getName()
            ),
                    params -> "The fallback language has to be \"required\"");
        }
    }

    private void eitherVocabularyReferenceOrTypeRequiredWithOptionalTranslationDefinitions(FieldDefinitionEntity fieldDefinitionEntity) throws VocabularyException {
        if (fieldDefinitionEntity.getReferenceVocabulary() != null) {
            if (fieldDefinitionEntity.getType() != null || !fieldDefinitionEntity.getTranslationDefinitions().isEmpty()) {
                List<String> errors = new ArrayList<>(2);
                if (fieldDefinitionEntity.getType() != null) {
                    errors.add("type specification not allowed");
                }
                if (!fieldDefinitionEntity.getTranslationDefinitions().isEmpty()) {
                    errors.add("translation specification not allowed");
                }
                throw new VocabularyException(FieldDefinitionReferenceVocabularyAndTypeNotPossible, null, Map.of(
                        "name", fieldDefinitionEntity.getName(),
                        "typeNotAllowed", String.valueOf(fieldDefinitionEntity.getType() != null),
                        "translationDefinitionsNotAllowed", String.valueOf(!fieldDefinitionEntity.getTranslationDefinitions().isEmpty())
                ),
                        params -> "You specified a reference vocabulary, therefore: " + String.join(", ", errors));
            }
        } else if (fieldDefinitionEntity.getType() == null) {
            throw new VocabularyException(FieldDefinitionNeitherReferenceVocabularyNorTypeNotPossible, null, Map.of(
                    "name", fieldDefinitionEntity.getName()
            ),
                    params -> "You did neither specify a reference vocabulary nor a type");
        }
    }
}
