package io.goobi.vocabularyserver.service.exchange;

import io.goobi.vocabularyserver.exchange.Vocabulary;
import io.goobi.vocabularyserver.model.VocabularyEntity;
import io.goobi.vocabularyserver.model.VocabularyRecordEntity;
import io.goobi.vocabularyserver.model.VocabularySchemaEntity;
import io.goobi.vocabularyserver.repositories.VocabularySchemaRepository;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.springframework.boot.test.context.SpringBootTest;

import java.util.List;
import java.util.Optional;

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

    private VocabularyEntity vocabulary;
    private Vocabulary vocabularyDTO;

    @BeforeEach
    void setUp() {
        VocabularySchemaEntity schema = new VocabularySchemaEntity();
        schema.setId(SCHEMA_ID);

        vocabulary = new VocabularyEntity();
        vocabulary.setId(VOCABULARY_ID);
        vocabulary.setSchema(schema);
        vocabulary.setName(VOCABULARY_NAME);
        vocabulary.setDescription(VOCABULARY_DESCRIPTION);

        VocabularyRecordEntity vocabularyRecord1 = new VocabularyRecordEntity();
        vocabularyRecord1.setId(RECORD_1_ID);
        vocabularyRecord1.setVocabulary(vocabulary);
        VocabularyRecordEntity vocabularyRecord2 = new VocabularyRecordEntity();
        vocabularyRecord2.setId(RECORD_2_ID);
        vocabularyRecord2.setVocabulary(vocabulary);

        vocabulary.setRecords(List.of(vocabularyRecord1, vocabularyRecord2));

        vocabularyDTO = new Vocabulary();
        vocabularyDTO.setId(VOCABULARY_ID);
        vocabularyDTO.setSchemaId(SCHEMA_ID);
        vocabularyDTO.setName(VOCABULARY_NAME);
        vocabularyDTO.setDescription(VOCABULARY_DESCRIPTION);

        when(vocabularySchemaRepository.findById(SCHEMA_ID)).thenReturn(Optional.of(schema));
    }

    @Test
    void validId_toDTO() {
        Vocabulary result = mapper.toDTO(vocabulary);

        assertEquals(VOCABULARY_ID, result.getId());
    }

    @Test
    void validSchemaId_toDTO() {
        Vocabulary result = mapper.toDTO(vocabulary);

        assertEquals(SCHEMA_ID, result.getSchemaId());
    }

    @Test
    void validName_toDTO() {
        Vocabulary result = mapper.toDTO(vocabulary);

        assertEquals(VOCABULARY_NAME, result.getName());
    }

    @Test
    void validDescription_toDTO() {
        Vocabulary result = mapper.toDTO(vocabulary);

        assertEquals(VOCABULARY_DESCRIPTION, result.getDescription());
    }

    @Test
    void validId_fromDTO() {
        VocabularyEntity result = mapper.toEntity(vocabularyDTO);

        assertEquals(VOCABULARY_ID, result.getId());
    }

    @Test
    void validSchemaId_fromDTO() {
        VocabularyEntity result = mapper.toEntity(vocabularyDTO);

        assertEquals(SCHEMA_ID, result.getSchema().getId());
    }

    @Test
    void validName_fromDTO() {
        VocabularyEntity result = mapper.toEntity(vocabularyDTO);

        assertEquals(VOCABULARY_NAME, result.getName());
    }

    @Test
    void validDescription_fromDTO() {
        VocabularyEntity result = mapper.toEntity(vocabularyDTO);

        assertEquals(VOCABULARY_DESCRIPTION, result.getDescription());
    }
}
