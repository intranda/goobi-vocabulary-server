package io.goobi.vocabulary.validation;

import io.goobi.vocabulary.exception.FieldDefinitionValidationException;
import io.goobi.vocabulary.model.jpa.FieldDefinitionEntity;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.List;

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

    private void ifTranslatedExactlyOneFallbackLanguageSpecified(FieldDefinitionEntity fieldDefinition) throws FieldDefinitionValidationException {
        if (!fieldDefinition.getTranslationDefinitions().isEmpty()) {
            long fallbacksDefined = fieldDefinition.getTranslationDefinitions().stream()
                    .filter(t -> Boolean.TRUE.equals(t.getFallback()))
                    .count();
            if (fallbacksDefined != 1) {
                throw new FieldDefinitionValidationException("Exactly one fallback language is required, but " + fallbacksDefined + " were specified");
            }
        }
    }

    private void fallbackLanguageMustBeRequired(FieldDefinitionEntity fieldDefinition) throws FieldDefinitionValidationException {
        if (fieldDefinition.getTranslationDefinitions().stream()
                .filter(t -> Boolean.TRUE.equals(t.getFallback()))
                .anyMatch(t -> !t.isRequired())) {
            throw new FieldDefinitionValidationException("The fallback language has to be \"required\"");
        }
    }

    private void eitherVocabularyReferenceOrTypeRequiredWithOptionalTranslationDefinitions(FieldDefinitionEntity fieldDefinitionEntity) throws FieldDefinitionValidationException {
        if (fieldDefinitionEntity.getReferenceVocabulary() != null) {
            if (fieldDefinitionEntity.getType() != null || !fieldDefinitionEntity.getTranslationDefinitions().isEmpty()) {
                List<String> errors = new ArrayList<>(2);
                if (fieldDefinitionEntity.getType() != null) {
                    errors.add("type specification not allowed");
                }
                if (!fieldDefinitionEntity.getTranslationDefinitions().isEmpty()) {
                    errors.add("translation specification not allowed");
                }
                throw new FieldDefinitionValidationException("You specified a reference vocabulary, therefore: " + String.join(", ", errors));
            }
        } else if (fieldDefinitionEntity.getType() == null) {
            throw new FieldDefinitionValidationException("You did not specify a type");
        }
    }
}
