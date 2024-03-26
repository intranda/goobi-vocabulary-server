package io.goobi.vocabularyserver.service.exchange;

import io.goobi.vocabularyserver.exchange.FieldInstanceDTO;
import io.goobi.vocabularyserver.model.FieldDefinition;
import io.goobi.vocabularyserver.model.FieldInstance;
import io.goobi.vocabularyserver.model.FieldType;
import io.goobi.vocabularyserver.model.Vocabulary;
import io.goobi.vocabularyserver.model.VocabularyRecord;
import io.goobi.vocabularyserver.model.VocabularySchema;
import io.goobi.vocabularyserver.repositories.FieldDefinitionRepository;
import io.goobi.vocabularyserver.repositories.VocabularyRecordRepository;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.springframework.boot.test.context.SpringBootTest;

import java.util.Optional;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.when;

@SpringBootTest
public class FieldInstanceMapperTest {
    private static final Long RECORD_ID = 406042345L;
    private static final Long FIELD_INSTANCE_ID = 20630634L;
    private static final Long FIELD_DEFINITION_ID = 9860234L;
    private static final String FIELD_INSTANCE_LANGUAGE = "eng";
    private static final String FIELD_INSTANCE_VALUE = "don't read this";

    private VocabularyRecord record;
    private FieldDefinition fieldDefinition;
    private FieldInstance fieldInstance;
    private FieldInstanceDTO fieldInstanceDTO;

    @Mock
    private FieldDefinitionRepository fieldDefinitionRepository;
    @Mock
    private VocabularyRecordRepository vocabularyRecordRepository;
    @InjectMocks
    private DTOMapperImpl mapper;

    @BeforeEach
    void setUp() {
        FieldType type = new FieldType();
        type.setName("Text");
        VocabularySchema schema = new VocabularySchema();
        Vocabulary vocabulary = new Vocabulary();
        vocabulary.setSchema(schema);
        vocabulary.setName("Vocab1");
        fieldDefinition = new FieldDefinition();
        fieldDefinition.setId(FIELD_DEFINITION_ID);
        fieldDefinition.setSchema(schema);
        fieldDefinition.setName("some name");
        fieldDefinition.setType(type);

        record = new VocabularyRecord();
        record.setVocabulary(vocabulary);
        record.setId(RECORD_ID);

        fieldInstance = new FieldInstance();
        fieldInstance.setId(FIELD_INSTANCE_ID);
        fieldInstance.setDefinition(fieldDefinition);
        fieldInstance.setVocabularyRecord(record);
        fieldInstance.setLanguage(FIELD_INSTANCE_LANGUAGE);
        fieldInstance.setValue(FIELD_INSTANCE_VALUE);

        fieldInstanceDTO = new FieldInstanceDTO();
        fieldInstanceDTO.setId(FIELD_INSTANCE_ID);
        fieldInstanceDTO.setRecordId(RECORD_ID);
        fieldInstanceDTO.setDefinitionId(FIELD_DEFINITION_ID);
        fieldInstanceDTO.setLanguage(FIELD_INSTANCE_LANGUAGE);
        fieldInstanceDTO.setValue(FIELD_INSTANCE_VALUE);

        when(fieldDefinitionRepository.findById(FIELD_DEFINITION_ID)).thenReturn(Optional.of(fieldDefinition));
        when(vocabularyRecordRepository.findById(RECORD_ID)).thenReturn(Optional.of(record));
    }

    @Test
    void validId_toDTO() {
        FieldInstanceDTO result = mapper.toDTO(fieldInstance);

        assertEquals(FIELD_INSTANCE_ID, result.getId());
    }

    @Test
    void validRecordId_toDTO() {
        FieldInstanceDTO result = mapper.toDTO(fieldInstance);

        assertEquals(RECORD_ID, result.getRecordId());
    }

    @Test
    void validDefinitionId_toDTO() {
        FieldInstanceDTO result = mapper.toDTO(fieldInstance);

        assertEquals(FIELD_DEFINITION_ID, result.getDefinitionId());
    }

    @Test
    void validLanguage_toDTO() {
        FieldInstanceDTO result = mapper.toDTO(fieldInstance);

        assertEquals(FIELD_INSTANCE_LANGUAGE, result.getLanguage());
    }

    @Test
    void validValue_toDTO() {
        FieldInstanceDTO result = mapper.toDTO(fieldInstance);

        assertEquals(FIELD_INSTANCE_VALUE, result.getValue());
    }

    @Test
    void validId_fromDTO() {
        FieldInstance result = mapper.toEntity(fieldInstanceDTO);

        assertEquals(FIELD_INSTANCE_ID, result.getId());
    }

    @Test
    void validRecordId_fromDTO() {
        FieldInstance result = mapper.toEntity(fieldInstanceDTO);

        assertEquals(RECORD_ID, result.getVocabularyRecord().getId());
    }

    @Test
    void validDefinitionId_fromDTO() {
        FieldInstance result = mapper.toEntity(fieldInstanceDTO);

        assertEquals(FIELD_DEFINITION_ID, result.getDefinition().getId());
    }

    @Test
    void validLanguage_fromDTO() {
        FieldInstance result = mapper.toEntity(fieldInstanceDTO);

        assertEquals(FIELD_INSTANCE_LANGUAGE, result.getLanguage());
    }

    @Test
    void validValue_fromDTO() {
        FieldInstance result = mapper.toEntity(fieldInstanceDTO);

        assertEquals(FIELD_INSTANCE_VALUE, result.getValue());
    }
}
