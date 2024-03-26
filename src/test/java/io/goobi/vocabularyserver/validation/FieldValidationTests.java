package io.goobi.vocabularyserver.validation;

import io.goobi.vocabularyserver.exception.ValidationException;
import io.goobi.vocabularyserver.model.FieldDefinition;
import io.goobi.vocabularyserver.model.FieldInstance;
import io.goobi.vocabularyserver.model.FieldType;
import io.goobi.vocabularyserver.model.SelectableValue;
import io.goobi.vocabularyserver.model.Vocabulary;
import io.goobi.vocabularyserver.model.VocabularyRecord;
import io.goobi.vocabularyserver.model.VocabularySchema;
import io.goobi.vocabularyserver.repositories.FieldInstanceRepository;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.springframework.boot.test.context.SpringBootTest;

import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.when;

@SpringBootTest
class FieldValidationTests {


    @Mock
    private FieldInstanceRepository fieldInstanceRepository;
    @InjectMocks
    private FieldValidatorImpl validator;

    private VocabularySchema schema;
    private VocabularyRecord record;


    @BeforeEach
    public void setUp() {
        schema = new VocabularySchema();
        Vocabulary vocabulary = new Vocabulary(schema, "Test vocabulary");
        record = new VocabularyRecord(vocabulary);
    }

    private FieldDefinition setupFieldDefinition(String name, String validation, List<String> selectableValues, boolean mainEntry, boolean titleField, boolean unique, boolean required) {
        FieldType type = new FieldType("test_type");
        type.setValidation(validation);
        if (selectableValues != null) {
            type.setSelectableValues(selectableValues.stream().map(SelectableValue::new).collect(Collectors.toSet()));
        }
        FieldDefinition definition = new FieldDefinition(schema, name, type);
        definition.setMainEntry(mainEntry);
        definition.setTitleField(titleField);
        definition.setUnique(unique);
        definition.setRequired(required);
        schema.setDefinitions(List.of(definition));
        return definition;
    }

    private FieldInstance setupFieldInstance(FieldDefinition definition, String value) {
        FieldInstance field = new FieldInstance(
                definition,
                record,
                value);
        record.setFields(Set.of(field));
        return field;
    }

    @Test
    void textFieldValueNotMatchingValidation_fails() {
        FieldInstance field = setupFieldInstance(
                setupFieldDefinition("name", "\\w+", null,true, true, true, true),
                "Thomas Lastname");

        assertThrows(ValidationException.class, () -> validator.validate(field));
    }

    @Test
    void textFieldValueMatchingValidation_success() throws ValidationException {
        FieldInstance field = setupFieldInstance(
                setupFieldDefinition("name", "\\w+", null,true, true, true, true),
                "Thomas");

        validator.validate(field);
    }

    @Test
    void numberFieldValueNotMatchingValidation_fails() {
        FieldInstance field = setupFieldInstance(
                setupFieldDefinition("age", "\\d+", null,true, true, true, true),
                "Thomas");

        assertThrows(ValidationException.class, () -> validator.validate(field));
    }

    @Test
    void numberFieldValueMatchingValidation_fails() throws ValidationException {
        FieldInstance field = setupFieldInstance(
                setupFieldDefinition("age", "\\d+", null,true, true, true, true),
                "32");

        validator.validate(field);
    }

    @Test
    void valueIsOneOfTheSelectableValues_success() throws ValidationException {
        FieldInstance field = setupFieldInstance(
                setupFieldDefinition("OS", null, List.of("Linux", "Windows"),false, false, false, false),
                "Linux");

        validator.validate(field);
    }

    @Test
    void valueIsNotOneOfTheSelectableValues_success() {
        FieldInstance field = setupFieldInstance(
                setupFieldDefinition("OS", null, List.of("Linux", "Windows"),false, false, false, false),
                "MacOS");

        assertThrows(ValidationException.class, () -> validator.validate(field));
    }

    @Test
    void emptyFieldValue_fails() {
        FieldInstance field = setupFieldInstance(
            setupFieldDefinition("hobbies", null, null,true, true, true, true),
            "");

        assertThrows(ValidationException.class, () -> validator.validate(field));
    }

    @Test
    void duplicateUniqueFieldRecord_fails() {
        when(fieldInstanceRepository.existsByVocabularyRecord_Vocabulary_IdAndValue(record.getVocabulary().getId(), "Bob")).thenReturn(true);

        FieldInstance field = setupFieldInstance(
                setupFieldDefinition("name", null, null,true, true, true, true),
                "Bob");

        assertThrows(ValidationException.class, () -> validator.validate(field));
    }
}
