package io.goobi.vocabularyserver.validation;

import io.goobi.vocabularyserver.exception.SchemaValidationException;
import io.goobi.vocabularyserver.model.FieldDefinitionEntity;
import io.goobi.vocabularyserver.model.VocabularySchemaEntity;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.stream.Collectors;

@Service
public class SchemaValidatorImpl extends BaseValidator<VocabularySchemaEntity> {
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

    private void checkFieldDefinitionExistence(VocabularySchemaEntity schema) throws SchemaValidationException {
        if (schema.getDefinitions().isEmpty()) {
            throw new SchemaValidationException("Empty field definitions are not allowed");
        }
    }

    private void checkSingleMainFieldDefinition(VocabularySchemaEntity schema) throws SchemaValidationException {
        List<String> mainFieldDefinitionNames = schema.getDefinitions().stream()
                .filter(d -> Boolean.TRUE.equals(d.getMainEntry()))
                .map(FieldDefinitionEntity::getName)
                .collect(Collectors.toList());
        if (mainFieldDefinitionNames.isEmpty()) {
            throw new SchemaValidationException("Exactly one main field required but none was specified");
        } else if (mainFieldDefinitionNames.size() > 1) {
            throw new SchemaValidationException("Exactly one main field required but the following multiple main fields are specified: " + String.join(", ", mainFieldDefinitionNames));
        }
    }

    private void checkMainFieldIsUnique(VocabularySchemaEntity schema) throws SchemaValidationException {
        List<FieldDefinitionEntity> mainEntries = schema.getDefinitions().stream()
                .filter(d -> Boolean.TRUE.equals(d.getMainEntry())).collect(Collectors.toList());
        if (!mainEntries.isEmpty() && mainEntries.stream()
                .filter(d -> Boolean.TRUE.equals(d.isUnique()))
                .count() != 1) {
            throw new SchemaValidationException("The main field needs to be set unique");
        }
    }

    private void checkMainFieldIsRequired(VocabularySchemaEntity schema) throws SchemaValidationException {
        List<FieldDefinitionEntity> mainEntries = schema.getDefinitions().stream()
                .filter(d -> Boolean.TRUE.equals(d.getMainEntry())).collect(Collectors.toList());
        if (!mainEntries.isEmpty() && mainEntries.stream()
                .filter(d -> Boolean.TRUE.equals(d.isRequired()))
                .count() != 1) {
            throw new SchemaValidationException("The main field needs to be set required");
        }
    }

    private void checkTitleFieldsAreRequired(VocabularySchemaEntity schema) throws SchemaValidationException {
        List<String> titleFieldsThatAreNotRequired = schema.getDefinitions().stream()
                .filter(d -> Boolean.TRUE.equals(d.isTitleField()))
                .filter(d -> Boolean.FALSE.equals(d.isRequired()))
                .map(FieldDefinitionEntity::getName)
                .collect(Collectors.toList());
        if (!titleFieldsThatAreNotRequired.isEmpty()) {
            throw new SchemaValidationException("Title fields need to be required: " + String.join(", ", titleFieldsThatAreNotRequired));
        }
    }
}
