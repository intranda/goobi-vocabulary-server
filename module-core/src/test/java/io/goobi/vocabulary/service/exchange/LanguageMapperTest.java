package io.goobi.vocabulary.service.exchange;

import io.goobi.vocabulary.exchange.Language;
import io.goobi.vocabulary.model.jpa.LanguageEntity;
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

    public static LanguageEntity createEntity(Long id, String abbreviation, String name) {
        LanguageEntity result = new LanguageEntity();
        result.setId(id);
        result.setAbbreviation(abbreviation);
        result.setName(name);
        return result;
    }

    public static Language createDTO(Long id, String abbreviation, String name) {
        Language result = new Language();
        result.setId(id);
        result.setAbbreviation(abbreviation);
        result.setName(name);
        return result;
    }

    @BeforeEach
    void setUp() {
        language = createEntity(LANGUAGE_ID, LANGUAGE_ABBREVIATION, LANGUAGE_NAME);
        languageDTO = createDTO(LANGUAGE_ID, LANGUAGE_ABBREVIATION, LANGUAGE_NAME);
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
