package io.goobi.vocabularyserver.service.exchange;

import io.goobi.vocabularyserver.exchange.FieldInstanceDTO;
import io.goobi.vocabularyserver.exchange.VocabularyRecordDTO;
import io.goobi.vocabularyserver.model.FieldDefinition;
import io.goobi.vocabularyserver.model.FieldInstance;
import io.goobi.vocabularyserver.model.FieldType;
import io.goobi.vocabularyserver.model.Vocabulary;
import io.goobi.vocabularyserver.model.VocabularyRecord;
import io.goobi.vocabularyserver.model.VocabularySchema;
import io.goobi.vocabularyserver.repositories.FieldDefinitionRepository;
import io.goobi.vocabularyserver.repositories.VocabularyRecordRepository;
import io.goobi.vocabularyserver.repositories.VocabularyRepository;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.springframework.boot.test.context.SpringBootTest;

import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.when;

@SpringBootTest
class VocabularyRecordMapperTest {
    private static final Long VOCABULARY_ID = 3052312L;
    private static final Long SCHEMA_ID = 64687L;
    private static final Long FIELD_DEFINITION_1_ID = 45234L;
    private static final String FIELD_DEFINITION_1_NAME = "First name";
    private static final Long FIELD_DEFINITION_2_ID = 423455234L;
    private static final String FIELD_DEFINITION_2_NAME = "Last name";
    private static final Long RECORD_ID = 2205723L;
    private static final Long FIELD_INSTANCE_1_ID = 235253L;
    private static final String FIELD_INSTANCE_1_VALUE = "Adam";
    private static final Long FIELD_INSTANCE_2_ID = 9841321L;
    private static final String FIELD_INSTANCE_2_VALUE = "Thompson";

    @Mock
    private VocabularyRepository vocabularyRepository;
    @Mock
    private VocabularyRecordRepository vocabularyRecordRepository;
    @Mock
    private FieldDefinitionRepository fieldDefinitionRepository;
    @InjectMocks
    private DTOMapperImpl mapper;

    private VocabularyRecord vocabularyRecord;
    private VocabularyRecordDTO vocabularyRecordDTO;
    private FieldInstance fieldInstance1;
    private FieldInstance fieldInstance2;
    private FieldInstanceDTO fieldInstanceDTO1;
    private FieldInstanceDTO fieldInstanceDTO2;

    @BeforeEach
    void setUp() {
        VocabularySchema schema = new VocabularySchema();
        schema.setId(SCHEMA_ID);

        FieldType type = new FieldType();
        type.setName("Text");

        FieldDefinition fieldDefinition1 = new FieldDefinition();
        fieldDefinition1.setId(FIELD_DEFINITION_1_ID);
        fieldDefinition1.setSchema(schema);
        fieldDefinition1.setName(FIELD_DEFINITION_1_NAME);
        fieldDefinition1.setType(type);

        FieldDefinition fieldDefinition2 = new FieldDefinition();
        fieldDefinition2.setId(FIELD_DEFINITION_2_ID);
        fieldDefinition2.setSchema(schema);
        fieldDefinition2.setName(FIELD_DEFINITION_2_NAME);
        fieldDefinition2.setType(type);

        schema.setDefinitions(List.of(fieldDefinition1, fieldDefinition2));

        Vocabulary vocabulary = new Vocabulary();
        vocabulary.setId(VOCABULARY_ID);
        vocabulary.setSchema(schema);
        vocabulary.setName("Some Vocabulary name");

        vocabularyRecord = new VocabularyRecord();
        vocabularyRecord.setId(RECORD_ID);
        vocabularyRecord.setVocabulary(vocabulary);

        fieldInstance1 = new FieldInstance();
        fieldInstance1.setId(FIELD_INSTANCE_1_ID);
        fieldInstance1.setDefinition(fieldDefinition1);
        fieldInstance1.setVocabularyRecord(vocabularyRecord);
//        fieldInstance1.setValue(FIELD_INSTANCE_1_VALUE);
        fieldInstance2 = new FieldInstance();
        fieldInstance2.setId(FIELD_INSTANCE_2_ID);
        fieldInstance2.setDefinition(fieldDefinition2);
        fieldInstance2.setVocabularyRecord(vocabularyRecord);
//        fieldInstance2.setValue(FIELD_INSTANCE_2_VALUE);

        vocabularyRecord.setFields(Set.of(fieldInstance1, fieldInstance2));

        fieldInstanceDTO1 = new FieldInstanceDTO();
        fieldInstanceDTO1.setId(FIELD_INSTANCE_1_ID);
        fieldInstanceDTO1.setRecordId(RECORD_ID);
        fieldInstanceDTO1.setDefinitionId(FIELD_DEFINITION_1_ID);
//        fieldInstanceDTO1.setValue(FIELD_INSTANCE_1_VALUE);
        fieldInstanceDTO2 = new FieldInstanceDTO();
        fieldInstanceDTO2.setId(FIELD_INSTANCE_2_ID);
        fieldInstanceDTO2.setRecordId(RECORD_ID);
        fieldInstanceDTO2.setDefinitionId(FIELD_DEFINITION_2_ID);
//        fieldInstanceDTO2.setValue(FIELD_INSTANCE_2_VALUE);

        vocabularyRecordDTO = new VocabularyRecordDTO();
        vocabularyRecordDTO.setId(RECORD_ID);
        vocabularyRecordDTO.setVocabularyId(VOCABULARY_ID);
        vocabularyRecordDTO.setFields(Set.of(fieldInstanceDTO1, fieldInstanceDTO2));

        when(vocabularyRepository.findById(VOCABULARY_ID)).thenReturn(Optional.of(vocabulary));
        when(vocabularyRecordRepository.findById(RECORD_ID)).thenReturn(Optional.of(vocabularyRecord));
        when(fieldDefinitionRepository.findById(FIELD_DEFINITION_1_ID)).thenReturn(Optional.of(fieldDefinition1));
        when(fieldDefinitionRepository.findById(FIELD_DEFINITION_2_ID)).thenReturn(Optional.of(fieldDefinition2));
    }

    @Test
    void validId_toDTO() {
        VocabularyRecordDTO result = mapper.toDTO(vocabularyRecord);

        assertEquals(RECORD_ID, result.getId());
    }

    @Test
    void validVocabularyId_toDTO() {
        VocabularyRecordDTO result = mapper.toDTO(vocabularyRecord);

        assertEquals(VOCABULARY_ID, result.getVocabularyId());
    }

    @Test
    void validChildFields_toDTO() {
        VocabularyRecordDTO result = mapper.toDTO(vocabularyRecord);

        assertFieldInstancesEquals(Set.of(fieldInstance1, fieldInstance2), result.getFields());
    }

    @Test
    void validId_fromDTO() {
        VocabularyRecord result = mapper.toEntity(vocabularyRecordDTO);

        assertEquals(RECORD_ID, result.getId());
    }

    @Test
    void validVocabularyId_fromDTO() {
        VocabularyRecord result = mapper.toEntity(vocabularyRecordDTO);

        assertEquals(VOCABULARY_ID, result.getVocabulary().getId());
    }

    @Test
    void validChildFields_fromDTO() {
        VocabularyRecord result = mapper.toEntity(vocabularyRecordDTO);

        assertFieldInstancesEquals(result.getFields(), Set.of(fieldInstanceDTO1, fieldInstanceDTO2));
    }

    private void assertFieldInstancesEquals(Set<FieldInstance> a, Set<FieldInstanceDTO> b) {
        assertEquals(a.size(), b.size());
        for (FieldInstanceDTO x : b) {
            List<FieldInstance> matches = a.stream().filter(aa -> aa.getId().equals(x.getId())).collect(Collectors.toList());
            assertEquals(1, matches.size());
            FieldInstance match = matches.get(0);
            assertEquals(x.getRecordId(), match.getVocabularyRecord().getId());
//            assertEquals(x.getValue(), match.getValue());
        }
    }
}
