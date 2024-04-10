package io.goobi.vocabularyserver.validation;

import io.goobi.vocabularyserver.exception.ValidationException;
import io.goobi.vocabularyserver.model.FieldDefinitionEntity;
import io.goobi.vocabularyserver.model.VocabularySchemaEntity;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.springframework.boot.test.context.SpringBootTest;

import java.util.List;

import static org.junit.jupiter.api.Assertions.assertThrows;

@SpringBootTest
class SchemaValidationTests {
    @InjectMocks
    private SchemaValidatorImpl validator;

    private static FieldDefinitionEntity validMainField(VocabularySchemaEntity schema, String name) {
        FieldDefinitionEntity mainEntryField = new FieldDefinitionEntity();
        mainEntryField.setSchema(schema);
        mainEntryField.setName(name);
        mainEntryField.setMainEntry(true);
        mainEntryField.setRequired(true);
        mainEntryField.setUnique(true);
        return mainEntryField;
    }

    @Test
    void noFieldDefinitions_fails() {
        VocabularySchemaEntity schema = new VocabularySchemaEntity();

        assertThrows(ValidationException.class, () -> validator.validate(schema));
    }

    @Test
    void noMainFieldDefinition_fails() {
        VocabularySchemaEntity schema = new VocabularySchemaEntity();

        FieldDefinitionEntity nonMainEntryField = new FieldDefinitionEntity();
        nonMainEntryField.setSchema(schema);
        nonMainEntryField.setName("Not main");

        schema.setDefinitions(List.of(nonMainEntryField));

        assertThrows(ValidationException.class, () -> validator.validate(schema));
    }

    @Test
    void exactlyOneMainFieldDefinition_success() throws ValidationException {
        VocabularySchemaEntity schema = new VocabularySchemaEntity();

        FieldDefinitionEntity mainEntryField = validMainField(schema, "Main");

        schema.setDefinitions(List.of(mainEntryField));

        validator.validate(schema);
    }

    @Test
    void twoMainFieldDefinitions_fails() {
        VocabularySchemaEntity schema = new VocabularySchemaEntity();

        schema.setDefinitions(List.of(validMainField(schema, "Main1"), validMainField(schema, "Main2")));

        assertThrows(ValidationException.class, () -> validator.validate(schema));
    }

    @Test
    void exactlyOneMainFieldDefinitionThatIsNotUnique_fails() {
        VocabularySchemaEntity schema = new VocabularySchemaEntity();

        FieldDefinitionEntity mainEntryField = validMainField(schema, "Main");
        mainEntryField.setUnique(false);

        schema.setDefinitions(List.of(mainEntryField));

        assertThrows(ValidationException.class, () -> validator.validate(schema));
    }

    @Test
    void exactlyOneMainFieldDefinitionThatIsNotRequired_fails() {
        VocabularySchemaEntity schema = new VocabularySchemaEntity();

        FieldDefinitionEntity mainEntryField = validMainField(schema, "Main");
        mainEntryField.setRequired(false);

        schema.setDefinitions(List.of(mainEntryField));

        assertThrows(ValidationException.class, () -> validator.validate(schema));
    }

    @Test
    void titleFieldsThatIsNotRequired_fails() {
        VocabularySchemaEntity schema = new VocabularySchemaEntity();

        FieldDefinitionEntity mainEntryField = validMainField(schema, "Main");
        FieldDefinitionEntity titleField = new FieldDefinitionEntity();
        titleField.setSchema(schema);
        titleField.setName("Title");
        titleField.setTitleField(true);
        titleField.setRequired(false);

        schema.setDefinitions(List.of(mainEntryField, titleField));

        assertThrows(ValidationException.class, () -> validator.validate(schema));
    }

    @Test
    void titleFieldsThatIsRequired_success() throws ValidationException {
        VocabularySchemaEntity schema = new VocabularySchemaEntity();

        FieldDefinitionEntity mainEntryField = validMainField(schema, "Main");
        FieldDefinitionEntity titleField = new FieldDefinitionEntity();
        titleField.setSchema(schema);
        titleField.setName("Title");
        titleField.setTitleField(true);
        titleField.setRequired(true);

        schema.setDefinitions(List.of(mainEntryField, titleField));

        validator.validate(schema);
    }
}
