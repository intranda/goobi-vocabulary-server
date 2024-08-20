package io.goobi.vocabulary.service.exchange;

import io.goobi.vocabulary.exchange.TranslationDefinition;
import io.goobi.vocabulary.model.jpa.FieldDefinitionEntity;
import io.goobi.vocabulary.model.jpa.LanguageEntity;
import io.goobi.vocabulary.model.jpa.TranslationDefinitionEntity;
import io.goobi.vocabulary.repositories.FieldDefinitionRepository;
import io.goobi.vocabulary.repositories.LanguageRepository;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.springframework.boot.test.context.SpringBootTest;

import java.util.List;
import java.util.Optional;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.when;

@SpringBootTest
class TranslationDefinitionMapperTest {
    private static final Long FIELD_DEFINITION_ID = 6135464L;
    private static final Long TRANSLATION_DEFINITION_ID = 9687435L;
    private static final Long ENGLISH_ID = 65424L;
    private static final String ENGLISH_ABBREVIATION = "eng";
    private static final String ENGLISH_NAME = "English";

    @Mock
    private LanguageRepository languageRepository;
    @Mock
    private FieldDefinitionRepository fieldDefinitionRepository;
    @InjectMocks
    private DTOMapperImpl mapper;

    private FieldDefinitionEntity fieldDefinition;
    private TranslationDefinitionEntity translationDefinition;
    private TranslationDefinition translationDefinitionDTO;
    private LanguageEntity english;

    @BeforeEach
    void setUp() {
        english = LanguageMapperTest.createEntity(ENGLISH_ID, ENGLISH_ABBREVIATION, ENGLISH_NAME);
        when(languageRepository.findByAbbreviation(ENGLISH_ABBREVIATION)).thenReturn(Optional.of(english));

        fieldDefinition = new FieldDefinitionEntity();
        fieldDefinition.setId(FIELD_DEFINITION_ID);
        translationDefinition = new TranslationDefinitionEntity();
        translationDefinition.setId(TRANSLATION_DEFINITION_ID);
        translationDefinition.setLanguage(english);
        translationDefinition.setFieldDefinition(fieldDefinition);
        fieldDefinition.setTranslationDefinitions(List.of(translationDefinition));

        translationDefinitionDTO = new TranslationDefinition();
        translationDefinitionDTO.setId(TRANSLATION_DEFINITION_ID);
        translationDefinitionDTO.setLanguage(ENGLISH_ABBREVIATION);
        translationDefinitionDTO.setDefinitionId(FIELD_DEFINITION_ID);

        when(fieldDefinitionRepository.findById(FIELD_DEFINITION_ID)).thenReturn(Optional.of(fieldDefinition));
    }

    @Test
    void validId_toDTO() {
        TranslationDefinition result = mapper.toDTO(translationDefinition);

        assertEquals(TRANSLATION_DEFINITION_ID, result.getId());
    }

    @Test
    void validDefinitionId_toDTO() {
        TranslationDefinition result = mapper.toDTO(translationDefinition);

        assertEquals(FIELD_DEFINITION_ID, result.getDefinitionId());
    }

    @Test
    void validLanguage_toDTO() {
        TranslationDefinition result = mapper.toDTO(translationDefinition);

        assertEquals(ENGLISH_ABBREVIATION, result.getLanguage());
    }

    @Test
    void fallbackFalse_toDTO() {
        translationDefinition.setFallback(false);

        TranslationDefinition result = mapper.toDTO(translationDefinition);

        assertFalse(result.getFallback());
    }

    @Test
    void fallbackTrue_toDTO() {
        translationDefinition.setFallback(true);

        TranslationDefinition result = mapper.toDTO(translationDefinition);

        assertTrue(result.getFallback());
    }

    @Test
    void requiredFalse_toDTO() {
        translationDefinition.setRequired(false);

        TranslationDefinition result = mapper.toDTO(translationDefinition);

        assertFalse(result.getRequired());
    }

    @Test
    void requiredTrue_toDTO() {
        translationDefinition.setRequired(true);

        TranslationDefinition result = mapper.toDTO(translationDefinition);

        assertTrue(result.getRequired());
    }

    @Test
    void validId_fromDTO() {
        TranslationDefinitionEntity result = mapper.toEntity(translationDefinitionDTO);

        assertEquals(TRANSLATION_DEFINITION_ID, result.getId());
    }

    @Test
    void validDefinitionId_fromDTO() {
        TranslationDefinitionEntity result = mapper.toEntity(translationDefinitionDTO);

        assertEquals(fieldDefinition, result.getFieldDefinition());
    }

    @Test
    void validLanguage_fromDTO() {
        TranslationDefinitionEntity result = mapper.toEntity(translationDefinitionDTO);

        assertEquals(english, result.getLanguage());
    }

    @Test
    void fallbackFalse_fromDTO() {
        translationDefinitionDTO.setFallback(false);

        TranslationDefinitionEntity result = mapper.toEntity(translationDefinitionDTO);

        assertNull(result.getFallback());
    }

    @Test
    void fallbackTrue_fromDTO() {
        translationDefinitionDTO.setFallback(true);

        TranslationDefinitionEntity result = mapper.toEntity(translationDefinitionDTO);

        assertTrue(result.getFallback());
    }

    @Test
    void requiredFalse_fromDTO() {
        translationDefinitionDTO.setRequired(false);

        TranslationDefinitionEntity result = mapper.toEntity(translationDefinitionDTO);

        assertFalse(result.isRequired());
    }

    @Test
    void requiredTrue_fromDTO() {
        translationDefinitionDTO.setRequired(true);

        TranslationDefinitionEntity result = mapper.toEntity(translationDefinitionDTO);

        assertTrue(result.isRequired());
    }
}
