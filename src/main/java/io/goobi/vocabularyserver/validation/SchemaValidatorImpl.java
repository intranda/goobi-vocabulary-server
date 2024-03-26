package io.goobi.vocabularyserver.validation;

import io.goobi.vocabularyserver.exception.SchemaValidationException;
import io.goobi.vocabularyserver.model.FieldDefinition;
import io.goobi.vocabularyserver.model.VocabularySchema;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.stream.Collectors;

@Service
public class SchemaValidatorImpl extends BaseValidator<VocabularySchema> {
    public SchemaValidatorImpl() {
        super("Schema");
        setValidations(List.of(
                this::checkFieldDefinitionExistence,
                this::checkSingleMainFieldDefinition,
                this::checkMainFieldIsUnique,
                this::checkMainFieldIsRequired,
                this::checkTitleFieldsAreRequired
        ));
    }

    private void checkFieldDefinitionExistence(VocabularySchema schema) throws SchemaValidationException {
        if (schema.getDefinitions().isEmpty()) {
            throw new SchemaValidationException("Empty field definitions are not allowed");
        }
    }

    private void checkSingleMainFieldDefinition(VocabularySchema schema) throws SchemaValidationException {
        List<String> mainFieldDefinitionNames = schema.getDefinitions().stream()
                .filter(d -> Boolean.TRUE.equals(d.getMainEntry()))
                .map(FieldDefinition::getName)
                .collect(Collectors.toList());
        if (mainFieldDefinitionNames.isEmpty()) {
            throw new SchemaValidationException("Exactly one main field required but none was specified");
        } else if (mainFieldDefinitionNames.size() > 1) {
            throw new SchemaValidationException("Exactly one main field required but the following multiple main fields are specified: " + String.join(", ", mainFieldDefinitionNames));
        }
    }

    private void checkMainFieldIsUnique(VocabularySchema schema) throws SchemaValidationException {
        List<FieldDefinition> mainEntries = schema.getDefinitions().stream()
                .filter(d -> Boolean.TRUE.equals(d.getMainEntry())).collect(Collectors.toList());
        if (!mainEntries.isEmpty() && mainEntries.stream()
                .filter(d -> Boolean.TRUE.equals(d.getUnique()))
                .count() != 1) {
            throw new SchemaValidationException("The main field needs to be set unique");
        }
    }

    private void checkMainFieldIsRequired(VocabularySchema schema) throws SchemaValidationException {
        List<FieldDefinition> mainEntries = schema.getDefinitions().stream()
                .filter(d -> Boolean.TRUE.equals(d.getMainEntry())).collect(Collectors.toList());
        if (!mainEntries.isEmpty() && mainEntries.stream()
                .filter(d -> Boolean.TRUE.equals(d.getRequired()))
                .count() != 1) {
            throw new SchemaValidationException("The main field needs to be set required");
        }
    }

    private void checkTitleFieldsAreRequired(VocabularySchema schema) throws SchemaValidationException {
        List<String> titleFieldsThatAreNotRequired = schema.getDefinitions().stream()
                .filter(d -> Boolean.TRUE.equals(d.getTitleField()))
                .filter(d -> Boolean.FALSE.equals(d.getRequired()))
                .map(FieldDefinition::getName)
                .collect(Collectors.toList());
        if (!titleFieldsThatAreNotRequired.isEmpty()) {
            throw new SchemaValidationException("Title fields need to be required: " + String.join(", ", titleFieldsThatAreNotRequired));
        }
    }
}
