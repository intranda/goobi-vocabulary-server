package io.goobi.vocabulary.validation;

import io.goobi.vocabulary.exception.ValidationException;
import io.goobi.vocabulary.model.jpa.FieldDefinitionEntity;
import io.goobi.vocabulary.model.jpa.FieldTypeEntity;
import io.goobi.vocabulary.model.jpa.LanguageEntity;
import io.goobi.vocabulary.model.jpa.TranslationDefinitionEntity;
import io.goobi.vocabulary.model.jpa.VocabularyEntity;
import io.goobi.vocabulary.model.jpa.VocabularySchemaEntity;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.springframework.boot.test.context.SpringBootTest;

import java.util.List;

import static org.junit.jupiter.api.Assertions.assertThrows;

@SpringBootTest
class FieldDefinitionValidationTests {
    @InjectMocks
    private FieldDefinitionValidatorImpl validator;

    private LanguageEntity english;
    private LanguageEntity german;
    private FieldDefinitionEntity field;
    private VocabularyEntity referenceVocabulary;
    private FieldTypeEntity type;

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
        referenceVocabulary = new VocabularyEntity();
        type = new FieldTypeEntity();
    }


    @Test
    void noLanguageDefinitionGiven_success() throws ValidationException {
        field.setType(type);

        validator.validate(field);
    }

    @Test
    void singleValidLanguageDefinitionGiven_success() throws ValidationException {
        field.setType(type);
        TranslationDefinitionEntity translationDefinition = new TranslationDefinitionEntity();
        translationDefinition.setId(168435354L);
        translationDefinition.setFieldDefinition(field);
        translationDefinition.setLanguage(english);
        translationDefinition.setFallback(true);
        translationDefinition.setRequired(true);
        field.setTranslationDefinitions(List.of(translationDefinition));

        validator.validate(field);
    }

    @Test
    void singleInvalidLanguageDefinitionGiven_notFallback_fails() {
        field.setType(type);
        TranslationDefinitionEntity translationDefinition = new TranslationDefinitionEntity();
        translationDefinition.setId(168435354L);
        translationDefinition.setFieldDefinition(field);
        translationDefinition.setLanguage(english);
        translationDefinition.setFallback(false);
        translationDefinition.setRequired(true);
        field.setTranslationDefinitions(List.of(translationDefinition));

        assertThrows(ValidationException.class, () -> validator.validate(field));
    }

    @Test
    void singleInvalidLanguageDefinitionGiven_notRequired_fails() {
        field.setType(type);
        TranslationDefinitionEntity translationDefinition = new TranslationDefinitionEntity();
        translationDefinition.setId(168435354L);
        translationDefinition.setFieldDefinition(field);
        translationDefinition.setLanguage(english);
        translationDefinition.setFallback(true);
        translationDefinition.setRequired(false);
        field.setTranslationDefinitions(List.of(translationDefinition));

        assertThrows(ValidationException.class, () -> validator.validate(field));
    }

    @Test
    void twoValidLanguageDefinitionsGiven_bothRequired_success() throws ValidationException {
        field.setType(type);
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
        field.setTranslationDefinitions(List.of(englishTranslationDefinition, germanTranslationDefinition));

        validator.validate(field);
    }

    @Test
    void twoInvalidLanguageDefinitionsGiven_bothFallback_fails()  {
        field.setType(type);
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
        field.setTranslationDefinitions(List.of(englishTranslationDefinition, germanTranslationDefinition));

        assertThrows(ValidationException.class, () -> validator.validate(field));
    }

    @Test
    void twoInvalidLanguageDefinitionsGiven_fallbackNotRequired_fails()  {
        field.setType(type);
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
        field.setTranslationDefinitions(List.of(englishTranslationDefinition, germanTranslationDefinition));

        assertThrows(ValidationException.class, () -> validator.validate(field));
    }

    @Test
    void referenceVocabularyGiven_typeGivenAsWell_fails() {
        field.setReferenceVocabulary(referenceVocabulary);
        field.setType(type);

        assertThrows(ValidationException.class, () -> validator.validate(field));
    }

    @Test
    void referenceVocabularyGiven_translationDefinitionsGivenAsWell_fails() {
        field.setReferenceVocabulary(referenceVocabulary);
        TranslationDefinitionEntity translationDefinition = new TranslationDefinitionEntity();
        translationDefinition.setId(168435354L);
        translationDefinition.setFieldDefinition(field);
        translationDefinition.setLanguage(english);
        translationDefinition.setFallback(true);
        translationDefinition.setRequired(true);
        field.setTranslationDefinitions(List.of(translationDefinition));

        assertThrows(ValidationException.class, () -> validator.validate(field));
    }
}
