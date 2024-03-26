package io.goobi.vocabularyserver.validation;

import io.goobi.vocabularyserver.exception.ValidationException;
import io.goobi.vocabularyserver.model.FieldDefinition;
import io.goobi.vocabularyserver.model.FieldType;
import io.goobi.vocabularyserver.model.Vocabulary;
import io.goobi.vocabularyserver.model.VocabularyRecord;
import io.goobi.vocabularyserver.model.VocabularySchema;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.springframework.boot.test.context.SpringBootTest;

import java.lang.reflect.Field;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertThrows;

@SpringBootTest
class SchemaValidationTests {
    @InjectMocks
    private SchemaValidatorImpl validator;

    private static FieldDefinition validMainField(VocabularySchema schema, String name) {
        FieldDefinition mainEntryField = new FieldDefinition();
        mainEntryField.setSchema(schema);
        mainEntryField.setName(name);
        mainEntryField.setMainEntry(true);
        mainEntryField.setRequired(true);
        mainEntryField.setUnique(true);
        return mainEntryField;
    }

    @Test
    void noFieldDefinitions_fails() {
        VocabularySchema schema = new VocabularySchema();

        assertThrows(ValidationException.class, () -> validator.validate(schema));
    }

    @Test
    void noMainFieldDefinition_fails() {
        VocabularySchema schema = new VocabularySchema();

        FieldDefinition nonMainEntryField = new FieldDefinition();
        nonMainEntryField.setSchema(schema);
        nonMainEntryField.setName("Not main");

        schema.setDefinitions(List.of(nonMainEntryField));

        assertThrows(ValidationException.class, () -> validator.validate(schema));
    }

    @Test
    void exactlyOneMainFieldDefinition_success() throws ValidationException {
        VocabularySchema schema = new VocabularySchema();

        FieldDefinition mainEntryField = validMainField(schema, "Main");

        schema.setDefinitions(List.of(mainEntryField));

        validator.validate(schema);
    }

    @Test
    void twoMainFieldDefinitions_fails() {
        VocabularySchema schema = new VocabularySchema();

        schema.setDefinitions(List.of(validMainField(schema, "Main1"), validMainField(schema, "Main2")));

        assertThrows(ValidationException.class, () -> validator.validate(schema));
    }

    @Test
    void exactlyOneMainFieldDefinitionThatIsNotUnique_fails() {
        VocabularySchema schema = new VocabularySchema();

        FieldDefinition mainEntryField = validMainField(schema, "Main");
        mainEntryField.setUnique(false);

        schema.setDefinitions(List.of(mainEntryField));

        assertThrows(ValidationException.class, () -> validator.validate(schema));
    }

    @Test
    void exactlyOneMainFieldDefinitionThatIsNotRequired_fails() {
        VocabularySchema schema = new VocabularySchema();

        FieldDefinition mainEntryField = validMainField(schema, "Main");
        mainEntryField.setRequired(false);

        schema.setDefinitions(List.of(mainEntryField));

        assertThrows(ValidationException.class, () -> validator.validate(schema));
    }

    @Test
    void titleFieldsThatIsNotRequired_fails() {
        VocabularySchema schema = new VocabularySchema();

        FieldDefinition mainEntryField = validMainField(schema, "Main");
        FieldDefinition titleField = new FieldDefinition();
        titleField.setSchema(schema);
        titleField.setName("Title");
        titleField.setTitleField(true);
        titleField.setRequired(false);

        schema.setDefinitions(List.of(mainEntryField, titleField));

        assertThrows(ValidationException.class, () -> validator.validate(schema));
    }

    @Test
    void titleFieldsThatIsRequired_success() throws ValidationException {
        VocabularySchema schema = new VocabularySchema();

        FieldDefinition mainEntryField = validMainField(schema, "Main");
        FieldDefinition titleField = new FieldDefinition();
        titleField.setSchema(schema);
        titleField.setName("Title");
        titleField.setTitleField(true);
        titleField.setRequired(true);

        schema.setDefinitions(List.of(mainEntryField, titleField));

        validator.validate(schema);
    }
}
