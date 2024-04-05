package io.goobi.vocabularyserver.validation;

import io.goobi.vocabularyserver.exception.ValidationException;
import io.goobi.vocabularyserver.model.FieldDefinition;
import io.goobi.vocabularyserver.model.FieldInstance;
import io.goobi.vocabularyserver.model.FieldType;
import io.goobi.vocabularyserver.model.Vocabulary;
import io.goobi.vocabularyserver.model.VocabularyRecord;
import io.goobi.vocabularyserver.model.VocabularySchema;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.springframework.boot.test.context.SpringBootTest;

import java.util.List;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertThrows;

@SpringBootTest
class RecordValidationTests {
    private static final Long FIELD_TYPE_TEXT_ID = 546L;
    private static final String FIELD_TYPE_TEXT_NAME = "Text";

    @Mock
    private Validator<FieldInstance> fieldInstanceValidator;
    @InjectMocks
    private RecordValidatorImpl validator;

    private VocabularySchema schema;
    private Vocabulary vocabulary;
    private FieldType ftText;
    private FieldDefinition fdName;


    @BeforeEach
    public void setUp() {
        ftText = new FieldType();
        ftText.setName(FIELD_TYPE_TEXT_NAME);
        ftText.setId(FIELD_TYPE_TEXT_ID);
        ftText.setValidation("\\w+");

        schema = new VocabularySchema();
        vocabulary = new Vocabulary();
        vocabulary.setSchema(schema);
        vocabulary.setName("Test vocabulary");

        fdName = new FieldDefinition();
        fdName.setSchema(schema);
        fdName.setName("Name");
        fdName.setType(ftText);
        fdName.setMainEntry(true);
        fdName.setTitleField(true);
        fdName.setUnique(true);
        fdName.setRequired(true);

        schema.setDefinitions(List.of(fdName));
    }

    @Test
    void missingRequiredField_fails() {
        VocabularyRecord record = new VocabularyRecord();
        record.setVocabulary(vocabulary);

        assertThrows(ValidationException.class, () -> validator.validate(record));
    }

    @Test
    void insertingUnspecifiedField_fails() {
        VocabularyRecord record = new VocabularyRecord();
        record.setVocabulary(vocabulary);

        FieldInstance name = new FieldInstance();
        name.setVocabularyRecord(record);
        name.setDefinition(fdName);

        FieldDefinition os = new FieldDefinition();
        os.setSchema(schema);
        os.setName("Name");
        os.setType(ftText);
        os.setMainEntry(true);
        os.setTitleField(true);
        os.setUnique(true);
        os.setRequired(true);

        FieldInstance unknown = new FieldInstance();
        unknown.setVocabularyRecord(record);
        unknown.setDefinition(os);

        record.setFields(List.of(name, unknown));

        assertThrows(ValidationException.class, () -> validator.validate(record));
    }

    @Test
    void hierarchicalRecordsIfNotEnabled_fails() {
        schema.setHierarchicalRecords(false);

        VocabularyRecord parent = new VocabularyRecord();
        parent.setVocabulary(vocabulary);
        FieldInstance parentNameField = new FieldInstance();
        parentNameField.setDefinition(fdName);
        parentNameField.setVocabularyRecord(parent);
        parent.setFields(List.of(parentNameField));

        VocabularyRecord child = new VocabularyRecord();
        child.setVocabulary(vocabulary);
        FieldInstance childNameField = new FieldInstance();
        childNameField.setDefinition(fdName);
        childNameField.setVocabularyRecord(child);
        child.setFields(List.of(childNameField));
        child.setParentRecord(parent);
        parent.setChildren(List.of(child));

        assertAll("Parent and child validations",
                () -> assertThrows(ValidationException.class, () -> validator.validate(parent)),
                () -> assertThrows(ValidationException.class, () -> validator.validate(child))
        );
    }

    @Test
    void hierarchicalRecordsIfEnabled_success() {
        schema.setHierarchicalRecords(true);

        VocabularyRecord parent = new VocabularyRecord();
        parent.setVocabulary(vocabulary);
        FieldInstance parentNameField = new FieldInstance();
        parentNameField.setId(1L);
        parentNameField.setDefinition(fdName);
        parentNameField.setVocabularyRecord(parent);
        parent.setFields(List.of(parentNameField));

        VocabularyRecord child = new VocabularyRecord();
        child.setVocabulary(vocabulary);
        FieldInstance childNameField = new FieldInstance();
        childNameField.setId(2L);
        childNameField.setDefinition(fdName);
        childNameField.setVocabularyRecord(child);
        child.setFields(List.of(childNameField));
        child.setParentRecord(parent);
        parent.setChildren(List.of(child));

        assertAll("Parent and child validations",
                () -> validator.validate(parent),
                () -> validator.validate(child)
        );
    }
}
