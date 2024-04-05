package io.goobi.vocabularyserver.service.exchange;

import io.goobi.vocabularyserver.exchange.FieldInstanceDTO;
import io.goobi.vocabularyserver.exchange.FieldValueDTO;
import io.goobi.vocabularyserver.model.FieldDefinition;
import io.goobi.vocabularyserver.model.FieldInstance;
import io.goobi.vocabularyserver.model.FieldTranslation;
import io.goobi.vocabularyserver.model.FieldType;
import io.goobi.vocabularyserver.model.FieldValue;
import io.goobi.vocabularyserver.model.Language;
import io.goobi.vocabularyserver.model.Vocabulary;
import io.goobi.vocabularyserver.model.VocabularyRecord;
import io.goobi.vocabularyserver.model.VocabularySchema;
import io.goobi.vocabularyserver.repositories.FieldDefinitionRepository;
import io.goobi.vocabularyserver.repositories.LanguageRepository;
import io.goobi.vocabularyserver.repositories.VocabularyRecordRepository;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.springframework.boot.test.context.SpringBootTest;

import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.when;

@SpringBootTest
class FieldInstanceMapperTest {
    private static final Long RECORD_ID = 406042345L;
    private static final Long FIELD_INSTANCE_ID = 20630634L;
    private static final Long FIELD_DEFINITION_ID = 9860234L;
    private static final String FIELD_INSTANCE_VALUE1 = "don't read this";
    private static final String FIELD_INSTANCE_VALUE2 = "whatever..";

    private VocabularyRecord record;
    private FieldDefinition fieldDefinition;
    private FieldValue fieldValue1;
    private FieldValueDTO fieldValue1DTO;
    private FieldValue fieldValue2;
    private FieldValueDTO fieldValue2DTO;
    private FieldInstance fieldInstance;
    private FieldInstanceDTO fieldInstanceDTO;

    @Mock
    private LanguageRepository languageRepository;
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

        Language english = new Language();
        english.setId(1L);
        english.setAbbreviation("eng");
        english.setName("English");

        when(languageRepository.findByAbbreviation("eng")).thenReturn(Optional.of(english));

        FieldTranslation fieldTranslation1 = new FieldTranslation();
        fieldTranslation1.setId(1L);
        fieldTranslation1.setLanguage(english);
        fieldTranslation1.setValue(FIELD_INSTANCE_VALUE1);
        fieldValue1 = new FieldValue();
        fieldValue1.setId(1L);
        fieldValue1.setTranslations(List.of(fieldTranslation1));
        fieldTranslation1.setFieldValue(fieldValue1);

        FieldTranslation fieldTranslation2 = new FieldTranslation();
        fieldTranslation2.setId(2L);
        fieldTranslation2.setLanguage(english);
        fieldTranslation2.setValue(FIELD_INSTANCE_VALUE2);
        fieldValue2 = new FieldValue();
        fieldValue2.setId(2L);
        fieldValue2.setTranslations(List.of(fieldTranslation2));
        fieldTranslation2.setFieldValue(fieldValue2);

        fieldValue1DTO = new FieldValueDTO();
        fieldValue1DTO.setId(1L);
        fieldValue1DTO.setFieldId(FIELD_INSTANCE_ID);
        fieldValue1DTO.setTranslations(Map.of("eng", FIELD_INSTANCE_VALUE1));

        fieldValue2DTO = new FieldValueDTO();
        fieldValue2DTO.setId(2L);
        fieldValue2DTO.setFieldId(FIELD_INSTANCE_ID);
        fieldValue2DTO.setTranslations(Map.of("eng", FIELD_INSTANCE_VALUE2));

        fieldInstance = new FieldInstance();
        fieldInstance.setId(FIELD_INSTANCE_ID);
        fieldInstance.setDefinition(fieldDefinition);
        fieldInstance.setVocabularyRecord(record);

        fieldInstanceDTO = new FieldInstanceDTO();
        fieldInstanceDTO.setId(FIELD_INSTANCE_ID);
        fieldInstanceDTO.setRecordId(RECORD_ID);
        fieldInstanceDTO.setDefinitionId(FIELD_DEFINITION_ID);

        when(fieldDefinitionRepository.findById(FIELD_DEFINITION_ID)).thenReturn(Optional.of(fieldDefinition));
        when(vocabularyRecordRepository.findById(RECORD_ID)).thenReturn(Optional.of(record));
    }

    private void setUpFieldValues(FieldValue... values) {
        fieldInstance.setFieldValues(List.of(values));
        Arrays.stream(values).forEach(v -> v.setFieldInstance(fieldInstance));
    }

    private void setUpFieldDTOValues(FieldValueDTO... values) {
        fieldInstanceDTO.setValues(Set.of(values));
        Arrays.stream(values).forEach(v -> v.setFieldId(FIELD_INSTANCE_ID));
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
    void validSingleValue_toDTO() {
        setUpFieldValues(fieldValue1);

        FieldInstanceDTO result = mapper.toDTO(fieldInstance);

        assertAll(
                "Assert correct values",
                () -> assertEquals(1, result.getValues().size()),
                () -> assertEquals(FIELD_INSTANCE_ID, result.getValues().stream()
                        .findAny()
                        .orElseThrow()
                        .getFieldId()),
                () -> assertDTOTranslations(result.getValues(), FIELD_INSTANCE_VALUE1)
        );
    }

    @Test
    void validMultiValue_toDTO() {
        setUpFieldValues(fieldValue1, fieldValue2);

        FieldInstanceDTO result = mapper.toDTO(fieldInstance);

        assertAll(
                "Assert correct values",
                () -> assertEquals(2, result.getValues().size()),
                () -> assertEquals(FIELD_INSTANCE_ID, result.getValues().stream()
                        .findAny()
                        .orElseThrow()
                        .getFieldId()),
                () -> assertDTOTranslations(result.getValues(), FIELD_INSTANCE_VALUE1, FIELD_INSTANCE_VALUE2)
        );
    }

    private void assertDTOTranslations(Set<FieldValueDTO> fieldValues, String... values) {
        Set<String> expectedValues = Set.of(values);
        Set<String> providedValues = fieldValues.stream()
                .flatMap(fv -> fv.getTranslations().values().stream())
                .collect(Collectors.toSet());

        assertEquals(expectedValues, providedValues);
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
    void validSingleValue_fromDTO() {
        setUpFieldDTOValues(fieldValue1DTO);

        FieldInstance result = mapper.toEntity(fieldInstanceDTO);

        assertAll(
                "Assert correct values",
                () -> assertEquals(1, result.getFieldValues().size()),
                () -> assertEquals(FIELD_INSTANCE_ID, result.getFieldValues().stream()
                        .findAny()
                        .orElseThrow()
                        .getFieldInstance()
                        .getId()),
                () -> assertEntityTranslations(result.getFieldValues(), FIELD_INSTANCE_VALUE1)
        );
    }

    @Test
    void validMultiValue_fromDTO() {
        setUpFieldDTOValues(fieldValue1DTO, fieldValue2DTO);

        FieldInstance result = mapper.toEntity(fieldInstanceDTO);

        assertAll(
                "Assert correct values",
                () -> assertEquals(2, result.getFieldValues().size()),
                () -> assertEquals(FIELD_INSTANCE_ID, result.getFieldValues().stream()
                        .findAny()
                        .orElseThrow()
                        .getFieldInstance()
                        .getId()),
                () -> assertEntityTranslations(result.getFieldValues(), FIELD_INSTANCE_VALUE1, FIELD_INSTANCE_VALUE2)
        );
    }

    private void assertEntityTranslations(List<FieldValue> fieldValues, String... values) {
        Set<String> expectedValues = Set.of(values);
        Set<String> providedValues = fieldValues.stream()
                .flatMap(fv -> fv.getTranslations().stream()
                        .map(FieldTranslation::getValue))
                .collect(Collectors.toSet());

        assertEquals(expectedValues, providedValues);
    }
}
