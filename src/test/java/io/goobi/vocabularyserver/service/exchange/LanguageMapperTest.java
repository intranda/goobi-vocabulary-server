package io.goobi.vocabularyserver.service.exchange;

import io.goobi.vocabularyserver.exchange.Language;
import io.goobi.vocabularyserver.model.LanguageEntity;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;

import static org.junit.jupiter.api.Assertions.assertEquals;

@SpringBootTest
class LanguageMapperTest {
    private static final Long LANGUAGE_ID = 98435L;
    private static final String LANGUAGE_ABBREVIATION = "eng";
    private static final String LANGUAGE_NAME = "English";

    @Autowired
    private DTOMapper mapper;

    private LanguageEntity language;
    private Language languageDTO;

    @BeforeEach
    void setUp() {
        language = new LanguageEntity();
        language.setId(LANGUAGE_ID);
        language.setAbbreviation(LANGUAGE_ABBREVIATION);
        language.setName(LANGUAGE_NAME);
        languageDTO = new Language();
        languageDTO.setId(LANGUAGE_ID);
        languageDTO.setAbbreviation(LANGUAGE_ABBREVIATION);
        languageDTO.setName(LANGUAGE_NAME);
    }

    @Test
    void validId_toDTO() {
        Language result = mapper.toDTO(language);

        assertEquals(LANGUAGE_ID, result.getId());
    }

    @Test
    void validAbbreviation_toDTO() {
        Language result = mapper.toDTO(language);

        assertEquals(LANGUAGE_ABBREVIATION, result.getAbbreviation());
    }

    @Test
    void validName_toDTO() {
        Language result = mapper.toDTO(language);

        assertEquals(LANGUAGE_NAME, result.getName());
    }

    @Test
    void validId_fromDTO() {
        LanguageEntity result = mapper.toEntity(languageDTO);

        assertEquals(LANGUAGE_ID, result.getId());
    }

    @Test
    void validAbbreviation_fromDTO() {
        LanguageEntity result = mapper.toEntity(languageDTO);

        assertEquals(LANGUAGE_ABBREVIATION, result.getAbbreviation());
    }

    @Test
    void validName_fromDTO() {
        LanguageEntity result = mapper.toEntity(languageDTO);

        assertEquals(LANGUAGE_NAME, result.getName());
    }
}
