package io.goobi.vocabulary.validation;

import io.goobi.vocabulary.exception.VocabularyException;
import io.goobi.vocabulary.model.jpa.FieldDefinitionEntity;
import io.goobi.vocabulary.model.jpa.FieldInstanceEntity;
import io.goobi.vocabulary.model.jpa.FieldTypeEntity;
import io.goobi.vocabulary.model.jpa.VocabularyEntity;
import io.goobi.vocabulary.model.jpa.VocabularyRecordEntity;
import io.goobi.vocabulary.model.jpa.VocabularySchemaEntity;
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
    private FieldDefinitionEntity fdOther;


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
        fdName.setId(1L);
        fdName.setSchema(schema);
        fdName.setName("Name");
        fdName.setType(ftText);
        fdName.setMainEntry(true);
        fdName.setTitleField(true);
        fdName.setUnique(true);
        fdName.setRequired(true);

        fdOther = new FieldDefinitionEntity();
        fdOther.setId(2L);
        fdOther.setSchema(schema);
        fdOther.setName("Other");
        fdOther.setType(ftText);
        fdOther.setMainEntry(false);
        fdOther.setTitleField(true);
        fdOther.setUnique(false);
        fdOther.setRequired(true);

        schema.setDefinitions(List.of(fdName, fdOther));
    }

    @Test
    void missingAllRequiredFields_fails() {
        VocabularyRecordEntity record = new VocabularyRecordEntity();
        record.setVocabulary(vocabulary);

        assertThrows(VocabularyException.class, () -> validator.validate(record));
    }

    @Test
    void missingOneRequiredField_fails() {
        VocabularyRecordEntity record = new VocabularyRecordEntity();
        record.setVocabulary(vocabulary);

        FieldInstanceEntity name = new FieldInstanceEntity();
        name.setVocabularyRecord(record);
        name.setDefinition(fdName);

        record.setFields(List.of(name));

        assertThrows(VocabularyException.class, () -> validator.validate(record));
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

        assertThrows(VocabularyException.class, () -> validator.validate(record));
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
                () -> assertThrows(VocabularyException.class, () -> validator.validate(parent)),
                () -> assertThrows(VocabularyException.class, () -> validator.validate(child))
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
        FieldInstanceEntity parentOtherField = new FieldInstanceEntity();
        parentOtherField.setId(2L);
        parentOtherField.setDefinition(fdOther);
        parentOtherField.setVocabularyRecord(parent);
        parent.setFields(List.of(parentNameField, parentOtherField));

        VocabularyRecordEntity child = new VocabularyRecordEntity();
        child.setVocabulary(vocabulary);
        FieldInstanceEntity childNameField = new FieldInstanceEntity();
        childNameField.setId(3L);
        childNameField.setDefinition(fdName);
        childNameField.setVocabularyRecord(child);
        FieldInstanceEntity childOtherField = new FieldInstanceEntity();
        childOtherField.setId(4L);
        childOtherField.setDefinition(fdOther);
        childOtherField.setVocabularyRecord(child);
        child.setFields(List.of(childNameField, childOtherField));
        child.setParentRecord(parent);
        parent.setChildren(List.of(child));

        assertAll("Parent and child validations",
                () -> validator.validate(parent),
                () -> validator.validate(child)
        );
    }
}
