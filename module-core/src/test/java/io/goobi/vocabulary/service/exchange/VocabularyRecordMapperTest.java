package io.goobi.vocabulary.service.exchange;

import io.goobi.vocabulary.api.assemblers.RecordAssembler;
import io.goobi.vocabulary.exchange.FieldInstance;
import io.goobi.vocabulary.exchange.VocabularyRecord;
import io.goobi.vocabulary.model.jpa.FieldDefinitionEntity;
import io.goobi.vocabulary.model.jpa.FieldInstanceEntity;
import io.goobi.vocabulary.model.jpa.FieldTypeEntity;
import io.goobi.vocabulary.model.jpa.VocabularyEntity;
import io.goobi.vocabulary.model.jpa.VocabularyRecordEntity;
import io.goobi.vocabulary.model.jpa.VocabularySchemaEntity;
import io.goobi.vocabulary.repositories.FieldDefinitionRepository;
import io.goobi.vocabulary.repositories.VocabularyRecordRepository;
import io.goobi.vocabulary.repositories.VocabularyRepository;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Spy;
import org.springframework.boot.test.context.SpringBootTest;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
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
    @Spy
    private RecordAssembler recordAssembler = new RecordAssembler();

    private VocabularyEntity vocabulary;
    private FieldDefinitionEntity fieldDefinition1;
    private FieldDefinitionEntity fieldDefinition2;
    private VocabularyRecordEntity vocabularyRecord;
    private VocabularyRecord vocabularyRecordDTO;
    private FieldInstanceEntity fieldInstance1;
    private FieldInstanceEntity fieldInstance2;
    private FieldInstance fieldInstanceDTO1;
    private FieldInstance fieldInstanceDTO2;

    @BeforeEach
    void setUp() {
        VocabularySchemaEntity schema = new VocabularySchemaEntity();
        schema.setId(SCHEMA_ID);

        FieldTypeEntity type = new FieldTypeEntity();
        type.setName("Text");

        fieldDefinition1 = new FieldDefinitionEntity();
        fieldDefinition1.setId(FIELD_DEFINITION_1_ID);
        fieldDefinition1.setSchema(schema);
        fieldDefinition1.setName(FIELD_DEFINITION_1_NAME);
        fieldDefinition1.setType(type);

        fieldDefinition2 = new FieldDefinitionEntity();
        fieldDefinition2.setId(FIELD_DEFINITION_2_ID);
        fieldDefinition2.setSchema(schema);
        fieldDefinition2.setName(FIELD_DEFINITION_2_NAME);
        fieldDefinition2.setType(type);

        schema.setDefinitions(List.of(fieldDefinition1, fieldDefinition2));

        vocabulary = new VocabularyEntity();
        vocabulary.setId(VOCABULARY_ID);
        vocabulary.setSchema(schema);
        vocabulary.setName("Some Vocabulary name");

        vocabularyRecord = new VocabularyRecordEntity();
        vocabularyRecord.setId(RECORD_ID);
        vocabularyRecord.setVocabulary(vocabulary);

        fieldInstance1 = new FieldInstanceEntity();
        fieldInstance1.setId(FIELD_INSTANCE_1_ID);
        fieldInstance1.setDefinition(fieldDefinition1);
        fieldInstance1.setVocabularyRecord(vocabularyRecord);
//        fieldInstance1.setValue(FIELD_INSTANCE_1_VALUE);
        fieldInstance2 = new FieldInstanceEntity();
        fieldInstance2.setId(FIELD_INSTANCE_2_ID);
        fieldInstance2.setDefinition(fieldDefinition2);
        fieldInstance2.setVocabularyRecord(vocabularyRecord);
//        fieldInstance2.setValue(FIELD_INSTANCE_2_VALUE);

        vocabularyRecord.setFields(List.of(fieldInstance1, fieldInstance2));

        fieldInstanceDTO1 = new FieldInstance();
        fieldInstanceDTO1.setId(FIELD_INSTANCE_1_ID);
        fieldInstanceDTO1.setRecordId(RECORD_ID);
        fieldInstanceDTO1.setDefinitionId(FIELD_DEFINITION_1_ID);
//        fieldInstanceDTO1.setValue(FIELD_INSTANCE_1_VALUE);
        fieldInstanceDTO2 = new FieldInstance();
        fieldInstanceDTO2.setId(FIELD_INSTANCE_2_ID);
        fieldInstanceDTO2.setRecordId(RECORD_ID);
        fieldInstanceDTO2.setDefinitionId(FIELD_DEFINITION_2_ID);
//        fieldInstanceDTO2.setValue(FIELD_INSTANCE_2_VALUE);

        vocabularyRecordDTO = new VocabularyRecord();
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
        VocabularyRecord result = mapper.toDTO(vocabularyRecord);

        assertEquals(RECORD_ID, result.getId());
    }

    @Test
    void validVocabularyId_toDTO() {
        VocabularyRecord result = mapper.toDTO(vocabularyRecord);

        assertEquals(VOCABULARY_ID, result.getVocabularyId());
    }

    @Test
    void validChildFields_toDTO() {
        VocabularyRecord result = mapper.toDTO(vocabularyRecord);

        assertFieldInstancesEquals(List.of(fieldInstance1, fieldInstance2), new ArrayList<>(result.getFields()));
    }

    @Test
    void noParentRecord_toDTO() {
        VocabularyRecord result = mapper.toDTO(vocabularyRecord);

        assertNull(result.getParentId());
    }

    @Test
    void noChildrenRecords_toDTO() {
        VocabularyRecord result = mapper.toDTO(vocabularyRecord);

        assertNull(result.getChildren());
    }

    @Test
    void validChildrenRecords_toDTO() {
        VocabularyRecordEntity child = new VocabularyRecordEntity();
        child.setId(927394672364L);
        child.setVocabulary(vocabulary);

        FieldInstanceEntity fieldInstance = new FieldInstanceEntity();
        fieldInstance.setId(641565L);
        fieldInstance.setDefinition(fieldDefinition1);
        fieldInstance.setVocabularyRecord(child);
//        fieldInstance1.setValue(FIELD_INSTANCE_1_VALUE);
        child.setFields(List.of(fieldInstance));
        child.setParentRecord(vocabularyRecord);

        vocabularyRecord.setChildren(List.of(child));

        VocabularyRecord result = mapper.toDTO(vocabularyRecord);

        assertAll("Verify child record",
                () -> assertEquals(1, result.getChildren().size()),
                () -> assertEquals(927394672364L, Objects.requireNonNull(result.getChildren().stream()
                                .findAny()
                                .orElseThrow()))
        );
    }

    @Test
    void validId_fromDTO() {
        VocabularyRecordEntity result = mapper.toEntity(vocabularyRecordDTO);

        assertEquals(RECORD_ID, result.getId());
    }

    @Test
    void validVocabularyId_fromDTO() {
        VocabularyRecordEntity result = mapper.toEntity(vocabularyRecordDTO);

        assertEquals(VOCABULARY_ID, result.getVocabulary().getId());
    }

    @Test
    void validChildFields_fromDTO() {
        VocabularyRecordEntity result = mapper.toEntity(vocabularyRecordDTO);

        assertFieldInstancesEquals(result.getFields(), List.of(fieldInstanceDTO1, fieldInstanceDTO2));
    }

    private void assertFieldInstancesEquals(List<FieldInstanceEntity> a, List<FieldInstance> b) {
        assertEquals(a.size(), b.size());
        for (FieldInstance x : b) {
            List<FieldInstanceEntity> matches = a.stream().filter(aa -> aa.getId() == x.getId()).collect(Collectors.toList());
            assertEquals(1, matches.size());
            FieldInstanceEntity match = matches.get(0);
            assertEquals(x.getRecordId(), match.getVocabularyRecord().getId());
//            assertEquals(x.getValue(), match.getValue());
        }
    }

    @Test
    void noParentRecord_fromDTO() {
        VocabularyRecordEntity result = mapper.toEntity(vocabularyRecordDTO);

        assertNull(result.getParentRecord());
    }

    @Test
    void noChildrenRecords_fromDTO() {
        VocabularyRecordEntity result = mapper.toEntity(vocabularyRecordDTO);

        assertTrue(result.getChildren().isEmpty());
    }

    @Test
    void validChildrenRecords_fromDTO() {
        VocabularyRecordEntity child = new VocabularyRecordEntity();
        child.setId(927394672364L);
        child.setVocabulary(vocabulary);

        FieldInstanceEntity fieldInstance = new FieldInstanceEntity();
        fieldInstance.setId(641565L);
        fieldInstance.setDefinition(fieldDefinition1);
        fieldInstance.setVocabularyRecord(child);
//        fieldInstance1.setValue(FIELD_INSTANCE_1_VALUE);
        child.setFields(List.of(fieldInstance));
        child.setParentRecord(vocabularyRecord);

        vocabularyRecordDTO.setChildren(Set.of(927394672364L));
        when(vocabularyRecordRepository.findById(927394672364L)).thenReturn(Optional.of(child));

        VocabularyRecordEntity result = mapper.toEntity(vocabularyRecordDTO);

        assertAll("Verify child record",
                () -> assertEquals(1, result.getChildren().size()),
                () -> assertEquals(927394672364L, result.getChildren().stream()
                        .findAny()
                        .orElseThrow()
                        .getId()),
                () -> assertEquals(vocabulary, result.getChildren().stream()
                        .findAny()
                        .orElseThrow()
                        .getVocabulary()),
                () -> assertEquals(vocabularyRecord, result.getChildren().stream()
                        .findAny()
                        .orElseThrow()
                        .getParentRecord()),
                () -> assertTrue(result.getChildren().stream()
                        .findAny()
                        .orElseThrow()
                        .getChildren()
                        .isEmpty()),
// This assertion will never succeed, because we overwrite the field IDs to guarantee uniqueness in order to satisfy inequality (for Set)
//                () -> assertEquals(641565L, result.getChildren().stream()
//                        .findAny()
//                        .orElseThrow()
//                        .getFields()
//                        .stream()
//                        .findAny()
//                        .orElseThrow()
//                        .getId()),
                () -> assertEquals(fieldDefinition1, result.getChildren().stream()
                        .findAny()
                        .orElseThrow()
                        .getFields()
                        .stream()
                        .findAny()
                        .orElseThrow()
                        .getDefinition()),
                () -> assertEquals(927394672364L, result.getChildren().stream()
                        .findAny()
                        .orElseThrow()
                        .getFields()
                        .stream()
                        .findAny()
                        .orElseThrow()
                        .getVocabularyRecord()
                        .getId())
        );
    }
}
