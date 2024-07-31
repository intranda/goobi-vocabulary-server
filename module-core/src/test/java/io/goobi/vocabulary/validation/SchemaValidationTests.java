package io.goobi.vocabulary.validation;

import io.goobi.vocabulary.exception.VocabularyException;
import io.goobi.vocabulary.model.jpa.FieldDefinitionEntity;
import io.goobi.vocabulary.model.jpa.VocabularySchemaEntity;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.springframework.boot.test.context.SpringBootTest;

import java.util.List;

import static org.junit.jupiter.api.Assertions.assertThrows;

@SpringBootTest
class SchemaValidationTests {
    @Mock
    private Validator<FieldDefinitionValidatorImpl> fieldDefinitionValidator;
    @InjectMocks
    private SchemaValidatorImpl validator;


    private static FieldDefinitionEntity allFalseField(VocabularySchemaEntity schema, String name) {
        FieldDefinitionEntity mainEntryField = new FieldDefinitionEntity();
        mainEntryField.setSchema(schema);
        mainEntryField.setName(name);
        mainEntryField.setMainEntry(false);
        mainEntryField.setRequired(false);
        mainEntryField.setUnique(false);
        mainEntryField.setTitleField(false);
        mainEntryField.setMultiValued(false);
        return mainEntryField;
    }

    private static FieldDefinitionEntity validMainField(VocabularySchemaEntity schema, String name) {
        FieldDefinitionEntity mainEntryField = allFalseField(schema, name);
        mainEntryField.setMainEntry(true);
        mainEntryField.setRequired(true);
        mainEntryField.setUnique(true);
        return mainEntryField;
    }

    @Test
    void noFieldDefinitions_fails() {
        VocabularySchemaEntity schema = new VocabularySchemaEntity();

        assertThrows(VocabularyException.class, () -> validator.validate(schema));
    }

    @Test
    void noMainFieldDefinition_fails() {
        VocabularySchemaEntity schema = new VocabularySchemaEntity();

        FieldDefinitionEntity nonMainEntryField = new FieldDefinitionEntity();
        nonMainEntryField.setSchema(schema);
        nonMainEntryField.setName("Not main");

        schema.setDefinitions(List.of(nonMainEntryField));

        assertThrows(VocabularyException.class, () -> validator.validate(schema));
    }

    @Test
    void exactlyOneMainFieldDefinition_success() throws VocabularyException {
        VocabularySchemaEntity schema = new VocabularySchemaEntity();

        FieldDefinitionEntity mainEntryField = validMainField(schema, "Main");

        schema.setDefinitions(List.of(mainEntryField));

        validator.validate(schema);
    }

    @Test
    void twoMainFieldDefinitions_fails() {
        VocabularySchemaEntity schema = new VocabularySchemaEntity();

        schema.setDefinitions(List.of(validMainField(schema, "Main1"), validMainField(schema, "Main2")));

        assertThrows(VocabularyException.class, () -> validator.validate(schema));
    }

    @Test
    void exactlyOneMainFieldDefinitionThatIsNotRequired_fails() {
        VocabularySchemaEntity schema = new VocabularySchemaEntity();

        FieldDefinitionEntity mainEntryField = validMainField(schema, "Main");
        mainEntryField.setRequired(false);

        schema.setDefinitions(List.of(mainEntryField));

        assertThrows(VocabularyException.class, () -> validator.validate(schema));
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

        assertThrows(VocabularyException.class, () -> validator.validate(schema));
    }

    @Test
    void titleFieldsThatIsRequired_success() throws VocabularyException {
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
