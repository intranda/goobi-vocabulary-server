package io.goobi.vocabulary.validation;

import io.goobi.vocabulary.exception.VocabularyException;
import io.goobi.vocabulary.model.jpa.FieldDefinitionEntity;
import io.goobi.vocabulary.model.jpa.VocabularySchemaEntity;
import org.springframework.stereotype.Service;

import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import static io.goobi.vocabulary.exception.VocabularyException.ErrorCode.SchemaValidationDefinitionIssues;
import static io.goobi.vocabulary.exception.VocabularyException.ErrorCode.SchemaValidationMainFieldIsNotRequired;
import static io.goobi.vocabulary.exception.VocabularyException.ErrorCode.SchemaValidationMissingMainField;
import static io.goobi.vocabulary.exception.VocabularyException.ErrorCode.SchemaValidationNoDefinitions;
import static io.goobi.vocabulary.exception.VocabularyException.ErrorCode.SchemaValidationTooManyMainFields;

@Service
public class SchemaValidatorImpl extends BaseValidator<VocabularySchemaEntity> {
    private final Validator<FieldDefinitionEntity> fieldDefinitionValidator;

    public SchemaValidatorImpl(Validator<FieldDefinitionEntity> fieldDefinitionValidator) {
        super("Schema");
        this.fieldDefinitionValidator = fieldDefinitionValidator;
        setValidations(List.of(
                this::checkFieldDefinitionExistence,
                this::checkSingleMainFieldDefinition,
                this::checkMainFieldIsRequired,
                this::perFieldDefinitionChecks
        ));
    }

    private void checkFieldDefinitionExistence(VocabularySchemaEntity schema) throws VocabularyException {
        if (schema.getDefinitions().isEmpty()) {
            throw new VocabularyException(SchemaValidationNoDefinitions, null, null, params -> "Empty field definitions are not allowed");
        }
    }

    private void checkSingleMainFieldDefinition(VocabularySchemaEntity schema) throws VocabularyException {
        List<String> mainFieldDefinitionNames = schema.getDefinitions().stream()
                .filter(d -> Boolean.TRUE.equals(d.getMainEntry()))
                .map(FieldDefinitionEntity::getName)
                .collect(Collectors.toList());
        if (mainFieldDefinitionNames.isEmpty()) {
            throw new VocabularyException(SchemaValidationMissingMainField, null, null, params -> "Exactly one main field required but none was specified");
        } else if (mainFieldDefinitionNames.size() > 1) {
            throw new VocabularyException(SchemaValidationTooManyMainFields, null, Map.of(
                    "mainFieldNames", String.join(",", mainFieldDefinitionNames)
            ),
                    params -> "Exactly one main field required but the following multiple main fields are specified: " + params.get("mainFieldNames"));
        }
    }

    private void checkMainFieldIsRequired(VocabularySchemaEntity schema) throws VocabularyException {
        List<FieldDefinitionEntity> mainEntries = schema.getDefinitions().stream()
                .filter(d -> Boolean.TRUE.equals(d.getMainEntry()))
                .collect(Collectors.toList());
        if (!mainEntries.isEmpty() && mainEntries.stream()
                .filter(d -> Boolean.TRUE.equals(d.isRequired()))
                .count() != 1) {
            throw new VocabularyException(SchemaValidationMainFieldIsNotRequired, null, Map.of(
                    "mainFieldNames", String.join(",", mainEntries.stream()
                            .map(FieldDefinitionEntity::getName)
                            .collect(Collectors.joining(",")))
            ),
                    params -> "The main field needs to be set required: " + params.get("mainFieldNames"));
        }
    }

    private void perFieldDefinitionChecks(VocabularySchemaEntity schema) throws VocabularyException {
        List<VocabularyException> errors = new LinkedList<>();
        for (FieldDefinitionEntity d : schema.getDefinitions()) {
            try {
                fieldDefinitionValidator.validate(d);
            } catch (VocabularyException e) {
                errors.add(e);
            }
        }
        if (!errors.isEmpty()) {
            throw new VocabularyException(SchemaValidationDefinitionIssues, errors, null, params -> "Error validating vocabulary schema");
        }
    }
}
