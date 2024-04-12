package io.goobi.vocabularyserver.validation;

import io.goobi.vocabularyserver.exception.ValidationException;
import io.goobi.vocabularyserver.model.FieldDefinitionEntity;
import io.goobi.vocabularyserver.model.LanguageEntity;
import io.goobi.vocabularyserver.model.TranslationDefinitionEntity;
import io.goobi.vocabularyserver.model.VocabularySchemaEntity;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.springframework.boot.test.context.SpringBootTest;

import java.util.List;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.assertThrows;

@SpringBootTest
class FieldDefinitionValidationTests {
    @InjectMocks
    private FieldDefinitionValidatorImpl validator;

    private LanguageEntity english;
    private LanguageEntity german;
    private FieldDefinitionEntity field;

    @BeforeEach
    void setUp() {
        english = new LanguageEntity();
        english.setId(7968435L);
        english.setName("English");
        english.setAbbreviation("en");
        english.setIsDefault(true);
        german = new LanguageEntity();
        german.setId(465658L);
        german.setName("German");
        german.setAbbreviation("de");
        german.setIsDefault(false);
        VocabularySchemaEntity schema = new VocabularySchemaEntity();
        field = new FieldDefinitionEntity();
        field.setSchema(schema);
        field.setName("Field");
        field.setMainEntry(true);
        field.setRequired(true);
        field.setUnique(true);
        field.setTitleField(false);
        field.setMultiValued(false);
        schema.setDefinitions(List.of(field));
    }


    @Test
    void noLanguageDefinitionGiven_success() throws ValidationException {
        validator.validate(field);
    }

    @Test
    void singleValidLanguageDefinitionGiven_success() throws ValidationException {
        TranslationDefinitionEntity translationDefinition = new TranslationDefinitionEntity();
        translationDefinition.setId(168435354L);
        translationDefinition.setFieldDefinition(field);
        translationDefinition.setLanguage(english);
        translationDefinition.setFallback(true);
        translationDefinition.setRequired(true);
        field.setTranslationDefinitions(Set.of(translationDefinition));

        validator.validate(field);
    }

    @Test
    void singleInvalidLanguageDefinitionGiven_notFallback_fails() {
        TranslationDefinitionEntity translationDefinition = new TranslationDefinitionEntity();
        translationDefinition.setId(168435354L);
        translationDefinition.setFieldDefinition(field);
        translationDefinition.setLanguage(english);
        translationDefinition.setFallback(false);
        translationDefinition.setRequired(true);
        field.setTranslationDefinitions(Set.of(translationDefinition));

        assertThrows(ValidationException.class, () -> validator.validate(field));
    }

    @Test
    void singleInvalidLanguageDefinitionGiven_notRequired_fails() {
        TranslationDefinitionEntity translationDefinition = new TranslationDefinitionEntity();
        translationDefinition.setId(168435354L);
        translationDefinition.setFieldDefinition(field);
        translationDefinition.setLanguage(english);
        translationDefinition.setFallback(true);
        translationDefinition.setRequired(false);
        field.setTranslationDefinitions(Set.of(translationDefinition));

        assertThrows(ValidationException.class, () -> validator.validate(field));
    }

    @Test
    void twoValidLanguageDefinitionsGiven_bothRequired_success() throws ValidationException {
        TranslationDefinitionEntity englishTranslationDefinition = new TranslationDefinitionEntity();
        englishTranslationDefinition.setId(168435354L);
        englishTranslationDefinition.setFieldDefinition(field);
        englishTranslationDefinition.setLanguage(english);
        englishTranslationDefinition.setFallback(true);
        englishTranslationDefinition.setRequired(true);
        TranslationDefinitionEntity germanTranslationDefinition = new TranslationDefinitionEntity();
        germanTranslationDefinition.setId(6546163L);
        germanTranslationDefinition.setFieldDefinition(field);
        germanTranslationDefinition.setLanguage(german);
        germanTranslationDefinition.setFallback(false);
        germanTranslationDefinition.setRequired(true);
        field.setTranslationDefinitions(Set.of(englishTranslationDefinition, germanTranslationDefinition));

        validator.validate(field);
    }

    @Test
    void twoInvalidLanguageDefinitionsGiven_bothFallback_fails()  {
        TranslationDefinitionEntity englishTranslationDefinition = new TranslationDefinitionEntity();
        englishTranslationDefinition.setId(168435354L);
        englishTranslationDefinition.setFieldDefinition(field);
        englishTranslationDefinition.setLanguage(english);
        englishTranslationDefinition.setFallback(true);
        englishTranslationDefinition.setRequired(true);
        TranslationDefinitionEntity germanTranslationDefinition = new TranslationDefinitionEntity();
        germanTranslationDefinition.setId(6546163L);
        germanTranslationDefinition.setFieldDefinition(field);
        germanTranslationDefinition.setLanguage(german);
        germanTranslationDefinition.setFallback(true);
        germanTranslationDefinition.setRequired(true);
        field.setTranslationDefinitions(Set.of(englishTranslationDefinition, germanTranslationDefinition));

        assertThrows(ValidationException.class, () -> validator.validate(field));
    }

    @Test
    void twoInvalidLanguageDefinitionsGiven_fallbackNotRequired_fails()  {
        TranslationDefinitionEntity englishTranslationDefinition = new TranslationDefinitionEntity();
        englishTranslationDefinition.setId(168435354L);
        englishTranslationDefinition.setFieldDefinition(field);
        englishTranslationDefinition.setLanguage(english);
        englishTranslationDefinition.setFallback(true);
        englishTranslationDefinition.setRequired(false);
        TranslationDefinitionEntity germanTranslationDefinition = new TranslationDefinitionEntity();
        germanTranslationDefinition.setId(6546163L);
        germanTranslationDefinition.setFieldDefinition(field);
        germanTranslationDefinition.setLanguage(german);
        germanTranslationDefinition.setFallback(false);
        germanTranslationDefinition.setRequired(true);
        field.setTranslationDefinitions(Set.of(englishTranslationDefinition, germanTranslationDefinition));

        assertThrows(ValidationException.class, () -> validator.validate(field));
    }
}
