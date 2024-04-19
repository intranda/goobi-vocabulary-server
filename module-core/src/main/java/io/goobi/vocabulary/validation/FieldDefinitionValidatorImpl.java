package io.goobi.vocabulary.validation;

import io.goobi.vocabulary.exception.FieldDefinitionValidationException;
import io.goobi.vocabulary.model.FieldDefinitionEntity;
import org.springframework.stereotype.Service;

import java.util.List;

@Service
public class FieldDefinitionValidatorImpl extends BaseValidator<FieldDefinitionEntity> {
    public FieldDefinitionValidatorImpl() {
        super("Definition");
        setValidations(List.of(
                this::ifTranslatedExactlyOneFallbackLanguageSpecified,
                this::fallbackLanguageMustBeRequired
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
}
