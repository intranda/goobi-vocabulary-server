package io.goobi.vocabulary.validation;

import io.goobi.vocabulary.exception.ValidationException;
import io.goobi.vocabulary.model.FieldDefinitionEntity;
import io.goobi.vocabulary.model.FieldInstanceEntity;
import io.goobi.vocabulary.model.FieldTypeEntity;
import io.goobi.vocabulary.model.VocabularyEntity;
import io.goobi.vocabulary.model.VocabularyRecordEntity;
import io.goobi.vocabulary.model.VocabularySchemaEntity;
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
    private Validator<FieldInstanceEntity> fieldInstanceValidator;
    @InjectMocks
    private RecordValidatorImpl validator;

    private VocabularySchemaEntity schema;
    private VocabularyEntity vocabulary;
    private FieldTypeEntity ftText;
    private FieldDefinitionEntity fdName;


    @BeforeEach
    public void setUp() {
        ftText = new FieldTypeEntity();
        ftText.setName(FIELD_TYPE_TEXT_NAME);
        ftText.setId(FIELD_TYPE_TEXT_ID);
        ftText.setValidation("\\w+");

        schema = new VocabularySchemaEntity();
        vocabulary = new VocabularyEntity();
        vocabulary.setSchema(schema);
        vocabulary.setName("Test vocabulary");

        fdName = new FieldDefinitionEntity();
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
        VocabularyRecordEntity record = new VocabularyRecordEntity();
        record.setVocabulary(vocabulary);

        assertThrows(ValidationException.class, () -> validator.validate(record));
    }

    @Test
    void insertingUnspecifiedField_fails() {
        VocabularyRecordEntity record = new VocabularyRecordEntity();
        record.setVocabulary(vocabulary);

        FieldInstanceEntity name = new FieldInstanceEntity();
        name.setVocabularyRecord(record);
        name.setDefinition(fdName);

        FieldDefinitionEntity os = new FieldDefinitionEntity();
        os.setSchema(schema);
        os.setName("Name");
        os.setType(ftText);
        os.setMainEntry(true);
        os.setTitleField(true);
        os.setUnique(true);
        os.setRequired(true);

        FieldInstanceEntity unknown = new FieldInstanceEntity();
        unknown.setVocabularyRecord(record);
        unknown.setDefinition(os);

        record.setFields(List.of(name, unknown));

        assertThrows(ValidationException.class, () -> validator.validate(record));
    }

    @Test
    void hierarchicalRecordsIfNotEnabled_fails() {
        schema.setHierarchicalRecords(false);

        VocabularyRecordEntity parent = new VocabularyRecordEntity();
        parent.setVocabulary(vocabulary);
        FieldInstanceEntity parentNameField = new FieldInstanceEntity();
        parentNameField.setDefinition(fdName);
        parentNameField.setVocabularyRecord(parent);
        parent.setFields(List.of(parentNameField));

        VocabularyRecordEntity child = new VocabularyRecordEntity();
        child.setVocabulary(vocabulary);
        FieldInstanceEntity childNameField = new FieldInstanceEntity();
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

        VocabularyRecordEntity parent = new VocabularyRecordEntity();
        parent.setVocabulary(vocabulary);
        FieldInstanceEntity parentNameField = new FieldInstanceEntity();
        parentNameField.setId(1L);
        parentNameField.setDefinition(fdName);
        parentNameField.setVocabularyRecord(parent);
        parent.setFields(List.of(parentNameField));

        VocabularyRecordEntity child = new VocabularyRecordEntity();
        child.setVocabulary(vocabulary);
        FieldInstanceEntity childNameField = new FieldInstanceEntity();
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
