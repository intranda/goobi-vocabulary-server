package io.goobi.vocabulary.service.exchange;

import io.goobi.vocabulary.exchange.FieldDefinition;
import io.goobi.vocabulary.exchange.TranslationDefinition;
import io.goobi.vocabulary.model.FieldDefinitionEntity;
import io.goobi.vocabulary.model.FieldTypeEntity;
import io.goobi.vocabulary.model.LanguageEntity;
import io.goobi.vocabulary.model.TranslationDefinitionEntity;
import io.goobi.vocabulary.model.VocabularySchemaEntity;
import io.goobi.vocabulary.repositories.FieldTypeRepository;
import io.goobi.vocabulary.repositories.LanguageRepository;
import io.goobi.vocabulary.repositories.VocabularySchemaRepository;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.springframework.boot.test.context.SpringBootTest;

import java.util.List;
import java.util.Optional;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.when;

@SpringBootTest
class FieldDefinitionMapperTest {
    private static final Long SCHEMA_ID = 5396823543L;
    private static final Long ENGLISH_ID = 69854635314L;
    private static final String ENGLISH_ABBREVIATION = "eng";
    private static final String ENGLISH_NAME = "English";
    private static final Long GERMAN_ID = 65768954L;
    private static final String GERMAN_ABBREVIATION = "ger";
    private static final String GERMAN_NAME = "Deutsch";
    private static final Long ENGLISH_TRANSLATION_DEFINITION_ID = 68543L;
    private static final Long GERMAN_TRANSLATION_DEFINITION_ID = 74654L;
    private static final Long FIELD_DEFINITION_ID = 6234824L;
    private static final String FIELD_DEFINITION_NAME = "SOME_NAME";
    private static final Long FIELD_TYPE_ID = 203763L;

    @Mock
    private FieldTypeRepository fieldTypeRepository;
    @Mock
    private VocabularySchemaRepository vocabularySchemaRepository;
    @Mock
    private LanguageRepository languageRepository;

    @InjectMocks
    private DTOMapperImpl mapper;

    private VocabularySchemaEntity schema;
    private FieldTypeEntity fieldType;
    private LanguageEntity english;
    private LanguageEntity german;
    private TranslationDefinitionEntity englishTranslationDefinition;
    private TranslationDefinitionEntity germanTranslationDefinition;
    private TranslationDefinition englishTranslationDefinitionDTO;
    private TranslationDefinition germanTranslationDefinitionDTO;
    private FieldDefinitionEntity fieldDefinition;
    private FieldDefinition fieldDefinitionDTO;

    @BeforeEach
    void setUp() {
        schema = new VocabularySchemaEntity();
        schema.setId(SCHEMA_ID);

        fieldType = new FieldTypeEntity();
        fieldType.setId(FIELD_TYPE_ID);

        english = LanguageMapperTest.createEntity(ENGLISH_ID, ENGLISH_ABBREVIATION, ENGLISH_NAME);
        german = LanguageMapperTest.createEntity(GERMAN_ID, GERMAN_ABBREVIATION, GERMAN_NAME);
        when(languageRepository.findByAbbreviation(ENGLISH_ABBREVIATION)).thenReturn(Optional.ofNullable(english));
        when(languageRepository.findByAbbreviation(GERMAN_ABBREVIATION)).thenReturn(Optional.ofNullable(german));

        FieldDefinitionEntity child = new FieldDefinitionEntity();
        child.setSchema(schema);
        child.setName("Child");
        child.setType(fieldType);
        schema.getDefinitions().add(child);

        englishTranslationDefinition = new TranslationDefinitionEntity();
        englishTranslationDefinition.setId(ENGLISH_TRANSLATION_DEFINITION_ID);
        englishTranslationDefinition.setLanguage(english);
        englishTranslationDefinition.setFallback(true);
        englishTranslationDefinition.setRequired(true);
        germanTranslationDefinition = new TranslationDefinitionEntity();
        germanTranslationDefinition.setId(GERMAN_TRANSLATION_DEFINITION_ID);
        germanTranslationDefinition.setLanguage(german);
        germanTranslationDefinition.setFallback(false);
        germanTranslationDefinition.setRequired(false);

        englishTranslationDefinitionDTO = new TranslationDefinition();
        englishTranslationDefinitionDTO.setId(ENGLISH_TRANSLATION_DEFINITION_ID);
        englishTranslationDefinitionDTO.setDefinitionId(FIELD_DEFINITION_ID);
        englishTranslationDefinitionDTO.setLanguage(ENGLISH_ABBREVIATION);
        englishTranslationDefinitionDTO.setFallback(true);
        englishTranslationDefinitionDTO.setRequired(true);
        germanTranslationDefinitionDTO = new TranslationDefinition();
        germanTranslationDefinitionDTO.setId(GERMAN_TRANSLATION_DEFINITION_ID);
        germanTranslationDefinitionDTO.setDefinitionId(FIELD_DEFINITION_ID);
        germanTranslationDefinitionDTO.setLanguage(GERMAN_ABBREVIATION);
        germanTranslationDefinitionDTO.setFallback(false);
        germanTranslationDefinitionDTO.setRequired(false);

        fieldDefinition = new FieldDefinitionEntity();
        fieldDefinition.setId(FIELD_DEFINITION_ID);
        fieldDefinition.setSchema(schema);
        fieldDefinition.setName(FIELD_DEFINITION_NAME);
        fieldDefinition.setType(fieldType);
        fieldDefinition.setTranslationDefinitions(List.of(englishTranslationDefinition, germanTranslationDefinition));
        englishTranslationDefinition.setFieldDefinition(fieldDefinition);
        germanTranslationDefinition.setFieldDefinition(fieldDefinition);
        fieldDefinitionDTO = new FieldDefinition();
        fieldDefinitionDTO.setId(FIELD_DEFINITION_ID);
        fieldDefinitionDTO.setSchemaId(SCHEMA_ID);
        fieldDefinitionDTO.setName(FIELD_DEFINITION_NAME);
        fieldDefinitionDTO.setTypeId(FIELD_TYPE_ID);
        fieldDefinitionDTO.setTranslationDefinitions(Set.of(englishTranslationDefinitionDTO, germanTranslationDefinitionDTO));

        when(fieldTypeRepository.findById(FIELD_TYPE_ID)).thenReturn(Optional.of(fieldType));
        when(vocabularySchemaRepository.findById(SCHEMA_ID)).thenReturn(Optional.of(schema));
    }

    @Test
    void validId_toDTO() {
        FieldDefinition result = mapper.toDTO(fieldDefinition);

        assertEquals(FIELD_DEFINITION_ID, result.getId());
    }

    @Test
    void validSchema_toDTO() {
        FieldDefinition result = mapper.toDTO(fieldDefinition);

        assertEquals(SCHEMA_ID, result.getSchemaId());
    }

    @Test
    void validName_toDTO() {
        FieldDefinition result = mapper.toDTO(fieldDefinition);

        assertEquals(FIELD_DEFINITION_NAME, result.getName());
    }

    @Test
    void validType_toDTO() {
        FieldDefinition result = mapper.toDTO(fieldDefinition);

        assertEquals(FIELD_TYPE_ID, result.getTypeId());
    }

    @Test
    void notRequired_toDTO() {
        fieldDefinition.setRequired(false);

        FieldDefinition result = mapper.toDTO(fieldDefinition);

        assertFalse(result.getRequired());
    }

    @Test
    void notUnique_toDTO() {
        fieldDefinition.setUnique(false);

        FieldDefinition result = mapper.toDTO(fieldDefinition);

        assertFalse(result.getUnique());
    }

    @Test
    void notMainEntry_toDTO() {
        fieldDefinition.setMainEntry(false);

        FieldDefinition result = mapper.toDTO(fieldDefinition);

        assertFalse(result.getMainEntry());
    }

    @Test
    void notTitleField_toDTO() {
        fieldDefinition.setTitleField(false);

        FieldDefinition result = mapper.toDTO(fieldDefinition);

        assertFalse(result.getTitleField());
    }

    @Test
    void notMultiValued_toDTO() {
        fieldDefinition.setMultiValued(false);

        FieldDefinition result = mapper.toDTO(fieldDefinition);

        assertFalse(result.getMultiValued());
    }

    @Test
    void isRequired_toDTO() {
        fieldDefinition.setRequired(true);

        FieldDefinition result = mapper.toDTO(fieldDefinition);

        assertTrue(result.getRequired());
    }

    @Test
    void isUnique_toDTO() {
        fieldDefinition.setUnique(true);

        FieldDefinition result = mapper.toDTO(fieldDefinition);

        assertTrue(result.getUnique());
    }

    @Test
    void isMainEntry_toDTO() {
        fieldDefinition.setMainEntry(true);

        FieldDefinition result = mapper.toDTO(fieldDefinition);

        assertTrue(result.getMainEntry());
    }

    @Test
    void isTitleField_toDTO() {
        fieldDefinition.setTitleField(true);

        FieldDefinition result = mapper.toDTO(fieldDefinition);

        assertTrue(result.getTitleField());
    }

    @Test
    void isMultiValued_toDTO() {
        fieldDefinition.setMultiValued(true);

        FieldDefinition result = mapper.toDTO(fieldDefinition);

        assertTrue(result.getMultiValued());
    }

    @Test
    void translations_bothMapped_toDTO() {
        FieldDefinition result = mapper.toDTO(fieldDefinition);

        assertEquals(2, result.getTranslationDefinitions().size());
    }

    @Test
    void translations_englishMapped_toDTO() {
        FieldDefinition result = mapper.toDTO(fieldDefinition);

        Optional<TranslationDefinition> english = result.getTranslationDefinitions().stream()
                .filter(td -> td.getLanguage().equals(ENGLISH_ABBREVIATION))
                .findFirst();
        assertAll("English mapping assertions",
                () -> assertTrue(english.isPresent()),
                () -> assertEquals(ENGLISH_TRANSLATION_DEFINITION_ID, english.orElseThrow().getId()),
                () -> assertEquals(FIELD_DEFINITION_ID, english.orElseThrow().getDefinitionId()),
                () -> assertEquals(ENGLISH_ABBREVIATION, english.orElseThrow().getLanguage()),
                () -> assertTrue(english.orElseThrow().getFallback()),
                () -> assertTrue(english.orElseThrow().getRequired())
        );
    }

    @Test
    void translations_germanMapped_toDTO() {
        FieldDefinition result = mapper.toDTO(fieldDefinition);

        Optional<TranslationDefinition> german = result.getTranslationDefinitions().stream()
                .filter(td -> td.getLanguage().equals(GERMAN_ABBREVIATION))
                .findFirst();
        assertAll("German mapping assertions",
                () -> assertTrue(german.isPresent()),
                () -> assertEquals(GERMAN_TRANSLATION_DEFINITION_ID, german.orElseThrow().getId()),
                () -> assertEquals(FIELD_DEFINITION_ID, german.orElseThrow().getDefinitionId()),
                () -> assertEquals(GERMAN_ABBREVIATION, german.orElseThrow().getLanguage()),
                () -> assertFalse(german.orElseThrow().getFallback()),
                () -> assertFalse(german.orElseThrow().getRequired())
        );
    }

    @Test
    void validId_fromDTO() {
        FieldDefinitionEntity result = mapper.toEntity(fieldDefinitionDTO);

        assertEquals(FIELD_DEFINITION_ID, result.getId());
    }

    @Test
    void validSchema_fromDTO() {
        FieldDefinitionEntity result = mapper.toEntity(fieldDefinitionDTO);

        assertEquals(schema, result.getSchema());
    }

    @Test
    void validName_fromDTO() {
        FieldDefinitionEntity result = mapper.toEntity(fieldDefinitionDTO);

        assertEquals(FIELD_DEFINITION_NAME, result.getName());
    }

    @Test
    void validType_fromDTO() {
        FieldDefinitionEntity result = mapper.toEntity(fieldDefinitionDTO);

        assertEquals(fieldType, result.getType());
    }

    @Test
    void notRequired_fromDTO() {
        fieldDefinitionDTO.setRequired(false);

        FieldDefinitionEntity result = mapper.toEntity(fieldDefinitionDTO);

        assertFalse(result.isRequired());
    }

    @Test
    void notUnique_fromDTO() {
        fieldDefinitionDTO.setUnique(false);

        FieldDefinitionEntity result = mapper.toEntity(fieldDefinitionDTO);

        assertFalse(result.isUnique());
    }

    @Test
    void notMainEntry_fromDTO() {
        fieldDefinitionDTO.setMainEntry(false);

        FieldDefinitionEntity result = mapper.toEntity(fieldDefinitionDTO);

        assertNull(result.getMainEntry());
    }

    @Test
    void notTitleField_fromDTO() {
        fieldDefinitionDTO.setTitleField(false);

        FieldDefinitionEntity result = mapper.toEntity(fieldDefinitionDTO);

        assertFalse(result.isTitleField());
    }

    @Test
    void notMultiValued_fromDTO() {
        fieldDefinitionDTO.setMultiValued(false);

        FieldDefinitionEntity result = mapper.toEntity(fieldDefinitionDTO);

        assertFalse(result.isMultiValued());
    }

    @Test
    void isRequired_fromDTO() {
        fieldDefinitionDTO.setRequired(true);

        FieldDefinitionEntity result = mapper.toEntity(fieldDefinitionDTO);

        assertTrue(result.isRequired());
    }

    @Test
    void isUnique_fromDTO() {
        fieldDefinitionDTO.setUnique(true);

        FieldDefinitionEntity result = mapper.toEntity(fieldDefinitionDTO);

        assertTrue(result.isUnique());
    }

    @Test
    void isMainEntry_fromDTO() {
        fieldDefinitionDTO.setMainEntry(true);

        FieldDefinitionEntity result = mapper.toEntity(fieldDefinitionDTO);

        assertTrue(result.getMainEntry());
    }

    @Test
    void isTitleField_fromDTO() {
        fieldDefinitionDTO.setTitleField(true);

        FieldDefinitionEntity result = mapper.toEntity(fieldDefinitionDTO);

        assertTrue(result.isTitleField());
    }

    @Test
    void isMultiValued_fromDTO() {
        fieldDefinitionDTO.setMultiValued(true);

        FieldDefinitionEntity result = mapper.toEntity(fieldDefinitionDTO);

        assertTrue(result.isMultiValued());
    }

    @Test
    void translations_bothMapped_fromDTO() {
        FieldDefinitionEntity result = mapper.toEntity(fieldDefinitionDTO);

        assertEquals(2, result.getTranslationDefinitions().size());
    }

    @Test
    void translations_englishMapped_fromDTO() {
        FieldDefinitionEntity result = mapper.toEntity(fieldDefinitionDTO);

        Optional<TranslationDefinitionEntity> english = result.getTranslationDefinitions().stream()
                .filter(td -> td.getLanguage().getAbbreviation().equals(ENGLISH_ABBREVIATION))
                .findFirst();
        assertAll("English mapping assertions",
                () -> assertTrue(english.isPresent()),
                () -> assertEquals(ENGLISH_TRANSLATION_DEFINITION_ID, english.orElseThrow().getId()),
                () -> assertEquals(FIELD_DEFINITION_ID, english.orElseThrow().getFieldDefinition().getId()),
                () -> assertEquals(this.english, english.orElseThrow().getLanguage()),
                () -> assertEquals(Boolean.TRUE, english.orElseThrow().getFallback()),
                () -> assertTrue(english.orElseThrow().isRequired())
        );
    }

    @Test
    void translations_germanMapped_fromDTO() {
        FieldDefinitionEntity result = mapper.toEntity(fieldDefinitionDTO);

        Optional<TranslationDefinitionEntity> german = result.getTranslationDefinitions().stream()
                .filter(td -> td.getLanguage().getAbbreviation().equals(GERMAN_ABBREVIATION))
                .findFirst();
        assertAll("German mapping assertions",
                () -> assertTrue(german.isPresent()),
                () -> assertEquals(GERMAN_TRANSLATION_DEFINITION_ID, german.orElseThrow().getId()),
                () -> assertEquals(FIELD_DEFINITION_ID, german.orElseThrow().getFieldDefinition().getId()),
                () -> assertEquals(this.german, german.orElseThrow().getLanguage()),
                () -> assertNull(german.orElseThrow().getFallback()),
                () -> assertFalse(german.orElseThrow().isRequired())
        );
    }
}
