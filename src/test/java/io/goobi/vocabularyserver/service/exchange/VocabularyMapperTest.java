package io.goobi.vocabularyserver.service.exchange;

import io.goobi.vocabularyserver.exchange.VocabularyDTO;
import io.goobi.vocabularyserver.exchange.VocabularyRecordDTO;
import io.goobi.vocabularyserver.model.Vocabulary;
import io.goobi.vocabularyserver.model.VocabularyRecord;
import io.goobi.vocabularyserver.model.VocabularySchema;
import io.goobi.vocabularyserver.repositories.VocabularySchemaRepository;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.springframework.boot.test.context.SpringBootTest;

import java.util.Optional;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.when;

@SpringBootTest
class VocabularyMapperTest {
    private static final Long VOCABULARY_ID = 984351L;
    private static final String VOCABULARY_NAME = "First vocabulary";
    private static final String VOCABULARY_DESCRIPTION = "This is my very first vocabulary after the REST API introduction.";
    private static final Long SCHEMA_ID = 1564986L;
    private static final Long RECORD_1_ID = 1469413L;
    private static final Long RECORD_2_ID = 62363L;

    @Mock
    private VocabularySchemaRepository vocabularySchemaRepository;
    @InjectMocks
    private DTOMapperImpl mapper;

    private Vocabulary vocabulary;
    private VocabularyDTO vocabularyDTO;
    private VocabularyRecord vocabularyRecord1;
    private VocabularyRecord vocabularyRecord2;
    private VocabularyRecordDTO vocabularyRecordDTO1;
    private VocabularyRecordDTO vocabularyRecordDTO2;

    @BeforeEach
    void setUp() {
        VocabularySchema schema = new VocabularySchema();
        schema.setId(SCHEMA_ID);

        vocabulary = new Vocabulary(schema, VOCABULARY_NAME);
        vocabulary.setId(VOCABULARY_ID);
        vocabulary.setDescription(VOCABULARY_DESCRIPTION);

        vocabularyRecord1 = new VocabularyRecord(vocabulary);
        vocabularyRecord1.setId(RECORD_1_ID);
        vocabularyRecord2 = new VocabularyRecord(vocabulary);
        vocabularyRecord2.setId(RECORD_2_ID);

        vocabulary.setRecords(Set.of(vocabularyRecord1, vocabularyRecord2));

        vocabularyDTO = new VocabularyDTO();
        vocabularyDTO.setId(VOCABULARY_ID);
        vocabularyDTO.setSchemaId(SCHEMA_ID);
        vocabularyDTO.setName(VOCABULARY_NAME);
        vocabularyDTO.setDescription(VOCABULARY_DESCRIPTION);

        when(vocabularySchemaRepository.findById(SCHEMA_ID)).thenReturn(Optional.of(schema));
    }

    @Test
    void validId_toDTO() {
        VocabularyDTO result = mapper.toDTO(vocabulary);

        assertEquals(VOCABULARY_ID, result.getId());
    }

    @Test
    void validSchemaId_toDTO() {
        VocabularyDTO result = mapper.toDTO(vocabulary);

        assertEquals(SCHEMA_ID, result.getSchemaId());
    }

    @Test
    void validName_toDTO() {
        VocabularyDTO result = mapper.toDTO(vocabulary);

        assertEquals(VOCABULARY_NAME, result.getName());
    }

    @Test
    void validDescription_toDTO() {
        VocabularyDTO result = mapper.toDTO(vocabulary);

        assertEquals(VOCABULARY_DESCRIPTION, result.getDescription());
    }

    @Test
    void validId_fromDTO() {
        Vocabulary result = mapper.toEntity(vocabularyDTO);

        assertEquals(VOCABULARY_ID, result.getId());
    }

    @Test
    void validSchemaId_fromDTO() {
        Vocabulary result = mapper.toEntity(vocabularyDTO);

        assertEquals(SCHEMA_ID, result.getSchema().getId());
    }

    @Test
    void validName_fromDTO() {
        Vocabulary result = mapper.toEntity(vocabularyDTO);

        assertEquals(VOCABULARY_NAME, result.getName());
    }

    @Test
    void validDescription_fromDTO() {
        Vocabulary result = mapper.toEntity(vocabularyDTO);

        assertEquals(VOCABULARY_DESCRIPTION, result.getDescription());
    }
}
