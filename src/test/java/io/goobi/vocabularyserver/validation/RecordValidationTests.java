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

import java.util.List;

import static org.junit.jupiter.api.Assertions.assertThrows;

@SpringBootTest
class RecordValidationTests {
    private static final Long FIELD_TYPE_TEXT_ID = 546L;
    private static final String FIELD_TYPE_TEXT_NAME = "Text";
    @InjectMocks
    private RecordValidatorImpl validator;

    private Vocabulary vocabulary;


    @BeforeEach
    public void setUp() {
        FieldType ftText = new FieldType();
        ftText.setName(FIELD_TYPE_TEXT_NAME);
        ftText.setId(FIELD_TYPE_TEXT_ID);
        ftText.setValidation("\\w+");

        VocabularySchema schema = new VocabularySchema();
        vocabulary = new Vocabulary();
        vocabulary.setSchema(schema);
        vocabulary.setName("Test vocabulary");

        FieldDefinition fdName = new FieldDefinition();
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
}
