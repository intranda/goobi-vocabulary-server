package io.goobi.vocabulary.service.exchange;

import io.goobi.vocabulary.exchange.FieldInstance;
import io.goobi.vocabulary.exchange.FieldValue;
import io.goobi.vocabulary.exchange.TranslationInstance;
import io.goobi.vocabulary.model.jpa.FieldDefinitionEntity;
import io.goobi.vocabulary.model.jpa.FieldInstanceEntity;
import io.goobi.vocabulary.model.jpa.FieldTranslationEntity;
import io.goobi.vocabulary.model.jpa.FieldTypeEntity;
import io.goobi.vocabulary.model.jpa.FieldValueEntity;
import io.goobi.vocabulary.model.jpa.LanguageEntity;
import io.goobi.vocabulary.model.jpa.VocabularyEntity;
import io.goobi.vocabulary.model.jpa.VocabularyRecordEntity;
import io.goobi.vocabulary.model.jpa.VocabularySchemaEntity;
import io.goobi.vocabulary.repositories.FieldDefinitionRepository;
import io.goobi.vocabulary.repositories.LanguageRepository;
import io.goobi.vocabulary.repositories.VocabularyRecordRepository;
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

    private VocabularyRecordEntity record;
    private FieldDefinitionEntity fieldDefinition;
    private FieldValueEntity fieldValue1;
    private FieldValue fieldValue1DTO;
    private FieldValueEntity fieldValue2;
    private FieldValue fieldValue2DTO;
    private FieldInstanceEntity fieldInstance;
    private FieldInstance fieldInstanceDTO;

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
        FieldTypeEntity type = new FieldTypeEntity();
        type.setName("Text");
        VocabularySchemaEntity schema = new VocabularySchemaEntity();
        VocabularyEntity vocabulary = new VocabularyEntity();
        vocabulary.setSchema(schema);
        vocabulary.setName("Vocab1");
        fieldDefinition = new FieldDefinitionEntity();
        fieldDefinition.setId(FIELD_DEFINITION_ID);
        fieldDefinition.setSchema(schema);
        fieldDefinition.setName("some name");
        fieldDefinition.setType(type);

        record = new VocabularyRecordEntity();
        record.setVocabulary(vocabulary);
        record.setId(RECORD_ID);

        LanguageEntity english = new LanguageEntity();
        english.setId(1L);
        english.setAbbreviation("eng");
        english.setName("English");

        when(languageRepository.findByAbbreviation("eng")).thenReturn(Optional.of(english));

        FieldTranslationEntity fieldTranslation1 = new FieldTranslationEntity();
        fieldTranslation1.setId(1L);
        fieldTranslation1.setLanguage(english);
        fieldTranslation1.setValue(FIELD_INSTANCE_VALUE1);
        fieldValue1 = new FieldValueEntity();
        fieldValue1.setId(1L);
        fieldValue1.setTranslations(List.of(fieldTranslation1));
        fieldTranslation1.setFieldValue(fieldValue1);

        FieldTranslationEntity fieldTranslation2 = new FieldTranslationEntity();
        fieldTranslation2.setId(2L);
        fieldTranslation2.setLanguage(english);
        fieldTranslation2.setValue(FIELD_INSTANCE_VALUE2);
        fieldValue2 = new FieldValueEntity();
        fieldValue2.setId(2L);
        fieldValue2.setTranslations(List.of(fieldTranslation2));
        fieldTranslation2.setFieldValue(fieldValue2);

        fieldValue1DTO = new FieldValue();
        fieldValue1DTO.setId(1L);
        fieldValue1DTO.setFieldId(FIELD_INSTANCE_ID);
        TranslationInstance translationInstance1 = new TranslationInstance();
        translationInstance1.setLanguage("eng");
        translationInstance1.setValue(FIELD_INSTANCE_VALUE1);
        fieldValue1DTO.setTranslations(List.of(translationInstance1));

        fieldValue2DTO = new FieldValue();
        fieldValue2DTO.setId(2L);
        fieldValue2DTO.setFieldId(FIELD_INSTANCE_ID);
        TranslationInstance translationInstance2 = new TranslationInstance();
        translationInstance2.setLanguage("eng");
        translationInstance2.setValue(FIELD_INSTANCE_VALUE2);
        fieldValue2DTO.setTranslations(List.of(translationInstance2));

        fieldInstance = new FieldInstanceEntity();
        fieldInstance.setId(FIELD_INSTANCE_ID);
        fieldInstance.setDefinition(fieldDefinition);
        fieldInstance.setVocabularyRecord(record);

        fieldInstanceDTO = new FieldInstance();
        fieldInstanceDTO.setId(FIELD_INSTANCE_ID);
        fieldInstanceDTO.setRecordId(RECORD_ID);
        fieldInstanceDTO.setDefinitionId(FIELD_DEFINITION_ID);

        when(fieldDefinitionRepository.findById(FIELD_DEFINITION_ID)).thenReturn(Optional.of(fieldDefinition));
        when(vocabularyRecordRepository.findById(RECORD_ID)).thenReturn(Optional.of(record));
    }

    private void setUpFieldValues(FieldValueEntity... values) {
        fieldInstance.setFieldValues(List.of(values));
        Arrays.stream(values).forEach(v -> v.setFieldInstance(fieldInstance));
    }

    private void setUpFieldDTOValues(FieldValue... values) {
        fieldInstanceDTO.setValues(Set.of(values));
        Arrays.stream(values).forEach(v -> v.setFieldId(FIELD_INSTANCE_ID));
    }

    @Test
    void validId_toDTO() {
        FieldInstance result = mapper.toDTO(fieldInstance);

        assertEquals(FIELD_INSTANCE_ID, result.getId());
    }

    @Test
    void validRecordId_toDTO() {
        FieldInstance result = mapper.toDTO(fieldInstance);

        assertEquals(RECORD_ID, result.getRecordId());
    }

    @Test
    void validDefinitionId_toDTO() {
        FieldInstance result = mapper.toDTO(fieldInstance);

        assertEquals(FIELD_DEFINITION_ID, result.getDefinitionId());
    }

    @Test
    void validSingleValue_toDTO() {
        setUpFieldValues(fieldValue1);

        FieldInstance result = mapper.toDTO(fieldInstance);

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

        FieldInstance result = mapper.toDTO(fieldInstance);

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

    private void assertDTOTranslations(Set<FieldValue> fieldValues, String... values) {
        Set<String> expectedValues = Set.of(values);
        Set<String> providedValues = fieldValues.stream()
                .flatMap(fv -> fv.getTranslations().stream())
                .map(TranslationInstance::getValue)
                .collect(Collectors.toSet());

        assertEquals(expectedValues, providedValues);
    }

    @Test
    void validId_fromDTO() {
        FieldInstanceEntity result = mapper.toEntity(fieldInstanceDTO);

        assertEquals(FIELD_INSTANCE_ID, result.getId());
    }

    @Test
    void validRecordId_fromDTO() {
        FieldInstanceEntity result = mapper.toEntity(fieldInstanceDTO);

        assertEquals(RECORD_ID, result.getVocabularyRecord().getId());
    }

    @Test
    void validDefinitionId_fromDTO() {
        FieldInstanceEntity result = mapper.toEntity(fieldInstanceDTO);

        assertEquals(FIELD_DEFINITION_ID, result.getDefinition().getId());
    }

    @Test
    void validSingleValue_fromDTO() {
        setUpFieldDTOValues(fieldValue1DTO);

        FieldInstanceEntity result = mapper.toEntity(fieldInstanceDTO);

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

        FieldInstanceEntity result = mapper.toEntity(fieldInstanceDTO);

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

    private void assertEntityTranslations(List<FieldValueEntity> fieldValues, String... values) {
        Set<String> expectedValues = Set.of(values);
        Set<String> providedValues = fieldValues.stream()
                .flatMap(fv -> fv.getTranslations().stream()
                        .map(FieldTranslationEntity::getValue))
                .collect(Collectors.toSet());

        assertEquals(expectedValues, providedValues);
    }
}
