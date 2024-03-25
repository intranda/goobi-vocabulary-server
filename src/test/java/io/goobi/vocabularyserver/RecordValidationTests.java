package io.goobi.vocabularyserver;

import io.goobi.vocabularyserver.exception.RecordValidationException;
import io.goobi.vocabularyserver.exception.ValidationException;
import io.goobi.vocabularyserver.exchange.VocabularyRecordDTO;
import io.goobi.vocabularyserver.model.FieldDefinition;
import io.goobi.vocabularyserver.model.FieldInstance;
import io.goobi.vocabularyserver.model.FieldType;
import io.goobi.vocabularyserver.model.Vocabulary;
import io.goobi.vocabularyserver.model.VocabularyRecord;
import io.goobi.vocabularyserver.model.VocabularySchema;
import io.goobi.vocabularyserver.repositories.VocabularyRecordRepository;
import io.goobi.vocabularyserver.service.exchange.DTOMapper;
import io.goobi.vocabularyserver.service.manager.RecordManager;
import io.goobi.vocabularyserver.validation.FieldValidatorImpl;
import io.goobi.vocabularyserver.validation.RecordValidator;
import io.goobi.vocabularyserver.validation.RecordValidatorImpl;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Spy;
import org.springframework.boot.test.context.SpringBootTest;

import java.util.List;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.when;

@SpringBootTest
public class RecordValidationTests {
    private static final Long FIELD_TYPE_ANYTHING_ID = 546546L;
    private static final String FIELD_TYPE_ANYTHING_NAME = "Anything";
    private static final Long FIELD_TYPE_TEXT_ID = 546L;
    private static final String FIELD_TYPE_TEXT_NAME = "Text";
    private static final Long FIELD_TYPE_NUMBER_ID = 68651L;
    private static final String FIELD_TYPE_NUMBER_NAME = "Number";
    @Mock
    private DTOMapper mapper;
    @Mock
    private VocabularyRecordRepository repository;
    @Spy
    private RecordValidator validator = new RecordValidatorImpl(new FieldValidatorImpl());
    @InjectMocks
    private RecordManager recordManager;

    private Vocabulary vocabulary;
    private VocabularySchema schema;
    private FieldType ftText;
    private FieldType ftAnything;
    private FieldType ftNumber;
    private FieldDefinition fdName;
    private FieldDefinition fdAge;
    private FieldDefinition fdHobbies;


    @BeforeEach
    public void setUp() {
        ftAnything = new FieldType(FIELD_TYPE_ANYTHING_NAME);
        ftAnything.setId(FIELD_TYPE_ANYTHING_ID);
        ftText = new FieldType(FIELD_TYPE_TEXT_NAME);
        ftText.setId(FIELD_TYPE_TEXT_ID);
        ftText.setValidation("\\w+");
        ftNumber = new FieldType(FIELD_TYPE_NUMBER_NAME);
        ftNumber.setId(FIELD_TYPE_NUMBER_ID);
        ftNumber.setValidation("\\d+");

        schema = new VocabularySchema();
        vocabulary = new Vocabulary(schema, "Test vocabulary");

        fdName = new FieldDefinition(schema, "Name", ftText);
        fdName.setMainEntry(true);
        fdName.setTitleField(true);
        fdName.setUnique(true);
        fdName.setRequired(true);

        fdAge = new FieldDefinition(schema, "Age", ftNumber);
        fdAge.setMainEntry(false);
        fdAge.setTitleField(false);
        fdAge.setUnique(false);
        fdAge.setRequired(true);

        fdHobbies = new FieldDefinition(schema, "Hobbies", ftAnything);
        fdHobbies.setMainEntry(false);
        fdHobbies.setTitleField(false);
        fdHobbies.setUnique(false);
        fdHobbies.setRequired(false);

        schema.setDefinitions(List.of(fdName, fdAge, fdHobbies));
    }

    @Test
    void validRecord_success() throws RecordValidationException {
        VocabularyRecordDTO dto = new VocabularyRecordDTO();
        VocabularyRecord record = new VocabularyRecord(vocabulary);
        when(mapper.toEntity(dto)).thenReturn(record);

        FieldInstance nameField = new FieldInstance(fdName, record, "Thomas");
        FieldInstance ageField = new FieldInstance(fdAge, record, "32");
        record.setFields(Set.of(nameField, ageField));

        recordManager.create(dto);
    }

    @Test
    void missingRequiredField_fails() {
        VocabularyRecordDTO dto = new VocabularyRecordDTO();
        VocabularyRecord record = new VocabularyRecord(vocabulary);
        when(mapper.toEntity(dto)).thenReturn(record);

        FieldInstance nameField = new FieldInstance(fdName, record, "Thomas");
        record.setFields(Set.of(nameField));

        assertThrows(ValidationException.class, () -> recordManager.create(dto));
    }

    @Test
    void textFieldValueNotMatchingValidation_fails() {
        VocabularyRecordDTO dto = new VocabularyRecordDTO();
        VocabularyRecord record = new VocabularyRecord(vocabulary);
        when(mapper.toEntity(dto)).thenReturn(record);

        FieldInstance nameField = new FieldInstance(fdName, record, "Thomas Lastname");
        record.setFields(Set.of(nameField));

        assertThrows(ValidationException.class, () -> recordManager.create(dto));
    }

    @Test
    void numberFieldValueNotMatchingValidation_fails() {
        VocabularyRecordDTO dto = new VocabularyRecordDTO();
        VocabularyRecord record = new VocabularyRecord(vocabulary);
        when(mapper.toEntity(dto)).thenReturn(record);

        FieldInstance ageField = new FieldInstance(fdAge, record, "Thomas");
        record.setFields(Set.of(ageField));

        assertThrows(ValidationException.class, () -> recordManager.create(dto));
    }

    @Test
    void emptyFieldValue_fails() {
        VocabularyRecordDTO dto = new VocabularyRecordDTO();
        VocabularyRecord record = new VocabularyRecord(vocabulary);
        when(mapper.toEntity(dto)).thenReturn(record);

        FieldInstance nameField = new FieldInstance(fdName, record, "Thomas");
        FieldInstance ageField = new FieldInstance(fdAge, record, "32");
        FieldInstance hobbyField = new FieldInstance(fdHobbies, record, "");
        record.setFields(Set.of(nameField, ageField, hobbyField));

        assertThrows(ValidationException.class, () -> recordManager.create(dto));
    }
}
