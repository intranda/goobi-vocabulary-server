package io.goobi.vocabulary.service.exchange;

import io.goobi.vocabulary.exchange.FieldValue;
import io.goobi.vocabulary.exchange.TranslationInstance;
import io.goobi.vocabulary.model.jpa.FieldInstanceEntity;
import io.goobi.vocabulary.model.jpa.FieldTranslationEntity;
import io.goobi.vocabulary.model.jpa.FieldValueEntity;
import io.goobi.vocabulary.model.jpa.LanguageEntity;
import io.goobi.vocabulary.repositories.FieldInstanceRepository;
import io.goobi.vocabulary.repositories.LanguageRepository;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.springframework.boot.test.context.SpringBootTest;

import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.Optional;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.when;

@SpringBootTest
class FieldValueMapperTest {
    private static final Long PARENT_FIELD_INSTANCE_ID = 729385235L;
    private static final Long FIELD_VALUE_ID = 1687986L;
    private static final Long ENGLISH_ID = 1L;
    private static final String ENGLISH_ABBREVIATION = "eng";
    private static final String ENGLISH_NAME = "English";
    private static final String ENGLISH_VALUE_TRANSLATION_VALUE = "Value";
    private static final Long GERMAN_ID = 2L;
    private static final String GERMAN_ABBREVIATION = "ger";
    private static final String GERMAN_NAME = "Deutsch";
    private static final String GERMAN_VALUE_TRANSLATION_VALUE = "Wert";

    private FieldValueEntity fieldValue;
    private FieldValue fieldValueDTO;

    private FieldTranslationEntity englishValueTranslation;
    private FieldTranslationEntity germanValueTranslation;
    private Map.Entry<String, String> englishValueTranslationDTO;
    private Map.Entry<String, String> germanValueTranslationDTO;

    @Mock
    private LanguageRepository languageRepository;
    @Mock
    private FieldInstanceRepository fieldInstanceRepository;
    @InjectMocks
    private DTOMapperImpl mapper;

    @BeforeEach
    void setUp() {
        FieldInstanceEntity parent = new FieldInstanceEntity();
        parent.setId(PARENT_FIELD_INSTANCE_ID);
        when(fieldInstanceRepository.findById(PARENT_FIELD_INSTANCE_ID)).thenReturn(Optional.of(parent));

        LanguageEntity english = new LanguageEntity();
        english.setId(ENGLISH_ID);
        english.setAbbreviation(ENGLISH_ABBREVIATION);
        english.setName(ENGLISH_NAME);

        LanguageEntity german = new LanguageEntity();
        german.setId(GERMAN_ID);
        german.setAbbreviation(GERMAN_ABBREVIATION);
        german.setName(GERMAN_NAME);

        when(languageRepository.findByAbbreviation(ENGLISH_ABBREVIATION)).thenReturn(Optional.of(english));
        when(languageRepository.findByAbbreviation(GERMAN_ABBREVIATION)).thenReturn(Optional.of(german));

        englishValueTranslation = new FieldTranslationEntity();
        englishValueTranslation.setId(1L);
        englishValueTranslation.setValue(ENGLISH_VALUE_TRANSLATION_VALUE);
        englishValueTranslation.setLanguage(english);
        englishValueTranslationDTO = new Map.Entry<>() {
            @Override
            public String getKey() {
                return ENGLISH_ABBREVIATION;
            }

            @Override
            public String getValue() {
                return ENGLISH_VALUE_TRANSLATION_VALUE;
            }

            @Override
            public String setValue(String value) {
                return null;
            }
        };
        germanValueTranslationDTO = new Map.Entry<>() {
            @Override
            public String getKey() {
                return GERMAN_ABBREVIATION;
            }

            @Override
            public String getValue() {
                return GERMAN_VALUE_TRANSLATION_VALUE;
            }

            @Override
            public String setValue(String value) {
                return null;
            }
        };

        germanValueTranslation = new FieldTranslationEntity();
        germanValueTranslation.setId(2L);
        germanValueTranslation.setValue(GERMAN_VALUE_TRANSLATION_VALUE);
        germanValueTranslation.setLanguage(german);

        fieldValue = new FieldValueEntity();
        fieldValue.setId(FIELD_VALUE_ID);
        fieldValue.setFieldInstance(parent);

        fieldValueDTO = new FieldValue();
        fieldValueDTO.setId(FIELD_VALUE_ID);
        fieldValueDTO.setFieldId(PARENT_FIELD_INSTANCE_ID);
    }

    private void setUpFieldValueDTOTranslations(FieldTranslationEntity... translations) {
        fieldValue.setTranslations(List.of(translations));
        Arrays.stream(translations).forEach(t -> t.setFieldValue(fieldValue));
    }

    private void setUpFieldValueDTOTranslations(Map.Entry<String, String>... translations) {Arrays.stream(translations).forEach(t -> {
        TranslationInstance translationInstance = new TranslationInstance();
        translationInstance.setLanguage(t.getKey());
        translationInstance.setValue(t.getValue());
        fieldValueDTO.getTranslations().add(translationInstance);
    });
    }

    @Test
    void validId_toDTO() {
        FieldValue result = mapper.toDTO(fieldValue);

        assertEquals(FIELD_VALUE_ID, result.getId());
    }

    @Test
    void validFieldInstanceId_toDTO() {
        FieldValue result = mapper.toDTO(fieldValue);

        assertEquals(PARENT_FIELD_INSTANCE_ID, result.getFieldId());
    }

    @Test
    void validSingleLanguageTranslation_toDTO() {
        setUpFieldValueDTOTranslations(englishValueTranslation);

        FieldValue result = mapper.toDTO(fieldValue);

        List<TranslationInstance> resultTranslations = result.getTranslations();
        assertAll(
                "Assert correct translations",
                () -> assertEquals(1, resultTranslations.size()),
                () -> assertTrue(resultTranslations.stream().anyMatch(t-> ENGLISH_ABBREVIATION.equals(t.getLanguage()))),
                () -> assertEquals(ENGLISH_VALUE_TRANSLATION_VALUE, resultTranslations.stream().filter(t-> ENGLISH_ABBREVIATION.equals(t.getLanguage())).findFirst().orElseThrow().getValue())
        );
    }

    @Test
    void validMultiLanguageTranslation_toDTO() {
        setUpFieldValueDTOTranslations(englishValueTranslation, germanValueTranslation);

        FieldValue result = mapper.toDTO(fieldValue);

        List<TranslationInstance> resultTranslations = result.getTranslations();
        assertAll(
                "Assert correct translations",
                () -> assertEquals(2, resultTranslations.size()),
                () -> assertTrue(resultTranslations.stream().anyMatch(t-> ENGLISH_ABBREVIATION.equals(t.getLanguage()))),
                () -> assertTrue(resultTranslations.stream().anyMatch(t-> GERMAN_ABBREVIATION.equals(t.getLanguage()))),
                () -> assertEquals(ENGLISH_VALUE_TRANSLATION_VALUE, resultTranslations.stream().filter(t-> ENGLISH_ABBREVIATION.equals(t.getLanguage())).findFirst().orElseThrow().getValue()),
                () -> assertEquals(GERMAN_VALUE_TRANSLATION_VALUE, resultTranslations.stream().filter(t-> GERMAN_ABBREVIATION.equals(t.getLanguage())).findFirst().orElseThrow().getValue())
        );
    }

    @Test
    void validId_fromDTO() {
        FieldValueEntity result = mapper.toEntity(fieldValueDTO);

        assertEquals(FIELD_VALUE_ID, result.getId());
    }

    @Test
    void validFieldInstanceId_fromDTO() {
        FieldValueEntity result = mapper.toEntity(fieldValueDTO);

        assertEquals(PARENT_FIELD_INSTANCE_ID, result.getFieldInstance().getId());
    }

    @Test
    void validSingleLanguageTranslation_fromDTO() {
        setUpFieldValueDTOTranslations(englishValueTranslationDTO);

        FieldValueEntity result = mapper.toEntity(fieldValueDTO);

        List<FieldTranslationEntity> resultTranslations = result.getTranslations();
        assertAll(
                "Assert correct translations",
                () -> assertEquals(1, resultTranslations.size()),
                () -> assertEquals(ENGLISH_ABBREVIATION, resultTranslations.stream()
                        .findAny()
                        .orElseThrow()
                        .getLanguage()
                        .getAbbreviation()),
                () -> assertEquals(ENGLISH_NAME, resultTranslations.stream()
                        .findAny()
                        .orElseThrow()
                        .getLanguage()
                        .getName()),
                () -> assertEquals(ENGLISH_VALUE_TRANSLATION_VALUE, resultTranslations.stream()
                        .findAny()
                        .orElseThrow()
                        .getValue())
        );
    }

    @Test
    void validMultiLanguageTranslation_fromDTO() {
        setUpFieldValueDTOTranslations(englishValueTranslationDTO, germanValueTranslationDTO);

        FieldValueEntity result = mapper.toEntity(fieldValueDTO);

        List<FieldTranslationEntity> resultTranslations = result.getTranslations();
        assertAll(
                "Assert correct translations",
                () -> assertEquals(2, resultTranslations.size()),
                () -> assertEquals(ENGLISH_ABBREVIATION, resultTranslations.stream()
                        .filter(t -> t.getLanguage().getAbbreviation().equals(ENGLISH_ABBREVIATION))
                        .findAny()
                        .orElseThrow()
                        .getLanguage()
                        .getAbbreviation()),
                () -> assertEquals(ENGLISH_NAME, resultTranslations.stream()
                        .filter(t -> t.getLanguage().getAbbreviation().equals(ENGLISH_ABBREVIATION))
                        .findAny()
                        .orElseThrow()
                        .getLanguage()
                        .getName()),
                () -> assertEquals(ENGLISH_VALUE_TRANSLATION_VALUE, resultTranslations.stream()
                        .filter(t -> t.getLanguage().getAbbreviation().equals(ENGLISH_ABBREVIATION))
                        .findAny()
                        .orElseThrow()
                        .getValue()),
                () -> assertEquals(GERMAN_ABBREVIATION, resultTranslations.stream()
                        .filter(t -> t.getLanguage().getAbbreviation().equals(GERMAN_ABBREVIATION))
                        .findAny()
                        .orElseThrow()
                        .getLanguage()
                        .getAbbreviation()),
                () -> assertEquals(GERMAN_NAME, resultTranslations.stream()
                        .filter(t -> t.getLanguage().getAbbreviation().equals(GERMAN_ABBREVIATION))
                        .findAny()
                        .orElseThrow()
                        .getLanguage()
                        .getName()),
                () -> assertEquals(GERMAN_VALUE_TRANSLATION_VALUE, resultTranslations.stream()
                        .filter(t -> t.getLanguage().getAbbreviation().equals(GERMAN_ABBREVIATION))
                        .findAny()
                        .orElseThrow()
                        .getValue())
        );
    }
}
