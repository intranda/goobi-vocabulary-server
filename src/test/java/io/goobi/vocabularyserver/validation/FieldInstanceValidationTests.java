package io.goobi.vocabularyserver.validation;

import io.goobi.vocabularyserver.exception.ValidationException;
import io.goobi.vocabularyserver.model.FieldDefinition;
import io.goobi.vocabularyserver.model.FieldInstance;
import io.goobi.vocabularyserver.model.FieldTranslation;
import io.goobi.vocabularyserver.model.FieldType;
import io.goobi.vocabularyserver.model.FieldValue;
import io.goobi.vocabularyserver.model.Language;
import io.goobi.vocabularyserver.model.SelectableValue;
import io.goobi.vocabularyserver.model.Vocabulary;
import io.goobi.vocabularyserver.model.VocabularyRecord;
import io.goobi.vocabularyserver.model.VocabularySchema;
import io.goobi.vocabularyserver.repositories.FieldInstanceRepository;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.data.util.Pair;

import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.when;

@SpringBootTest
class FieldInstanceValidationTests {
    @Mock
    private FieldInstanceRepository fieldInstanceRepository;
    @InjectMocks
    private FieldInstanceValidatorImpl validator;

    private VocabularySchema schema;
    private VocabularyRecord record;


    @BeforeEach
    public void setUp() {
        schema = new VocabularySchema();
        Vocabulary vocabulary = new Vocabulary();
        vocabulary.setSchema(schema);
        vocabulary.setName("Test vocabulary");
        record = new VocabularyRecord();
        record.setVocabulary(vocabulary);
    }

    private FieldDefinition setupFieldDefinition(String name, String validation, List<String> selectableValues, boolean mainEntry, boolean titleField, boolean unique, boolean required, boolean multiValued) {
        FieldType type = new FieldType();
        type.setValidation(validation);
        if (selectableValues != null) {
            type.setSelectableValues(selectableValues.stream().map(s -> {
                SelectableValue sv = new SelectableValue();
                sv.setValue(s);
                return sv;
            }).collect(Collectors.toSet()));
        }
        FieldDefinition definition = new FieldDefinition();
        definition.setSchema(schema);
        definition.setName(name);
        definition.setType(type);
        definition.setMainEntry(mainEntry);
        definition.setTitleField(titleField);
        definition.setUnique(unique);
        definition.setRequired(required);
        definition.setMultiValued(multiValued);
        schema.setDefinitions(List.of(definition));
        return definition;
    }

    private FieldInstance setupFieldInstance(FieldDefinition definition, Pair<String, String>... values) {
        FieldInstance field = new FieldInstance();
        field.setDefinition(definition);
        field.setVocabularyRecord(record);
        field.setFieldValues(transform(field, values));
        record.setFields(Set.of(field));
        return field;
    }

    private Set<FieldValue> transform(FieldInstance field, Pair<String, String>[] values) {
        Map<String, Set<String>> valuesPerLanguage = new HashMap<>();
        for (Pair<String, String> p : values) {
            if (!valuesPerLanguage.containsKey(p.getFirst())) {
                valuesPerLanguage.put(p.getFirst(), new HashSet<>());
            }
            valuesPerLanguage.get(p.getFirst()).add(p.getSecond());
        }
        return valuesPerLanguage.entrySet().stream()
                .collect(Collectors.toMap(
                        e -> createLanguage(e.getKey()),
                        Map.Entry::getValue))
                .entrySet().stream()
                .flatMap(e -> e.getValue().stream()
                        .map(v -> createFieldValue(field, e.getKey(), v)))
                .collect(Collectors.toSet());
    }

    private Language createLanguage(String abbreviation) {
        Language language = new Language();
        language.setAbbreviation(abbreviation);
        return language;
    }

    private FieldValue createFieldValue(FieldInstance field, Language language, String value) {
        FieldValue fieldValue = new FieldValue();
        fieldValue.setFieldInstance(field);
        fieldValue.setTranslations(Set.of(createTranslation(fieldValue, language, value)));
        return fieldValue;
    }

    private FieldTranslation createTranslation(FieldValue field, Language language, String value) {
        FieldTranslation translation = new FieldTranslation();
        translation.setFieldValue(field);
        translation.setLanguage(language);
        translation.setValue(value);
        return translation;
    }

    @Test
    void textFieldValueNotMatchingValidation_fails() {
        FieldInstance field = setupFieldInstance(
                setupFieldDefinition("name", "\\w+", null, true, true, true, true, false),
                Pair.of("none", "Thomas Lastname"));

        assertThrows(ValidationException.class, () -> validator.validate(field));
    }

    @Test
    void textFieldValueMatchingValidation_success() throws ValidationException {
        FieldInstance field = setupFieldInstance(
                setupFieldDefinition("name", "\\w+", null, true, true, true, true, false),
                Pair.of("none", "Thomas"));

        validator.validate(field);
    }

    @Test
    void numberFieldValueNotMatchingValidation_fails() {
        FieldInstance field = setupFieldInstance(
                setupFieldDefinition("age", "\\d+", null, true, true, true, true, false),
                Pair.of("none", "Thomas"));

        assertThrows(ValidationException.class, () -> validator.validate(field));
    }

    @Test
    void numberFieldValueMatchingValidation_fails() throws ValidationException {
        FieldInstance field = setupFieldInstance(
                setupFieldDefinition("age", "\\d+", null, true, true, true, true, false),
                Pair.of("none", "32"));

        validator.validate(field);
    }

    @Test
    void valueIsOneOfTheSelectableValues_success() throws ValidationException {
        FieldInstance field = setupFieldInstance(
                setupFieldDefinition("OS", null, List.of("Linux", "Windows"), false, false, false, false, false),
                Pair.of("none", "Linux"));

        validator.validate(field);
    }

    @Test
    void valueIsNotOneOfTheSelectableValues_success() {
        FieldInstance field = setupFieldInstance(
                setupFieldDefinition("OS", null, List.of("Linux", "Windows"), false, false, false, false, false),
                Pair.of("none", "MacOS"));

        assertThrows(ValidationException.class, () -> validator.validate(field));
    }

    @Test
    void emptyFieldValue_fails() {
        FieldInstance field = setupFieldInstance(
                setupFieldDefinition("hobbies", null, null, true, true, true, true, false),
                Pair.of("none", ""));

        assertThrows(ValidationException.class, () -> validator.validate(field));
    }

    @Test
    void duplicateUniqueFieldValueForAnotherFieldDefinition_success() throws ValidationException {
        when(fieldInstanceRepository.existsByVocabularyRecord_Vocabulary_IdAndDefinition_IdAndFieldValues_Translations_Value(record.getVocabulary().getId(), 1L, "Bob")).thenReturn(true);
        when(fieldInstanceRepository.existsByVocabularyRecord_Vocabulary_IdAndDefinition_IdAndFieldValues_Translations_Value(record.getVocabulary().getId(), 2L, "Bob")).thenReturn(false);

        FieldInstance fieldFriend = setupFieldInstance(
                setupFieldDefinition("Best Friend", null, null, false, true, false, true, false),
                Pair.of("none", "Thomas"));
        fieldFriend.getDefinition().setId(1L);
        FieldInstance fieldName = setupFieldInstance(
                setupFieldDefinition("Name", null, null, true, true, true, true, false),
                Pair.of("none", "Bob"));
        fieldName.getDefinition().setId(2L);

        validator.validate(fieldFriend);
        validator.validate(fieldName);
    }

    @Test
    void duplicateUniqueFieldValueForTheSameFieldDefinition_fails() {
        when(fieldInstanceRepository.existsByVocabularyRecord_Vocabulary_IdAndDefinition_IdAndFieldValues_Translations_Value(record.getVocabulary().getId(), 1L, "Bob")).thenReturn(false);
        when(fieldInstanceRepository.existsByVocabularyRecord_Vocabulary_IdAndDefinition_IdAndFieldValues_Translations_Value(record.getVocabulary().getId(), 2L, "Bob")).thenReturn(true);

        FieldInstance fieldFriend = setupFieldInstance(
                setupFieldDefinition("Best Friend", null, null, false, true, false, true, false),
                Pair.of("none", "Thomas"));
        fieldFriend.getDefinition().setId(1L);
        FieldInstance fieldName = setupFieldInstance(
                setupFieldDefinition("Name", null, null, true, true, true, true, false),
                Pair.of("none", "Bob"));
        fieldName.getDefinition().setId(2L);

        assertThrows(ValidationException.class, () -> validator.validate(fieldName));
    }

    @Test
    void multipleFieldValuesIfMultiValuedIsDisabled_fails() {
        FieldInstance multiValuedInstance = setupFieldInstance(
                setupFieldDefinition("Fruit", null, null, false, true, false, true, false),
                Pair.of("none", "Apple"), Pair.of("none", "Banana"));

        assertThrows(ValidationException.class, () -> validator.validate(multiValuedInstance));
    }

    @Test
    void multipleFieldValuesIfMultiValuedIsEnabled_success() throws ValidationException {
        FieldInstance multiValuedInstance = setupFieldInstance(
                setupFieldDefinition("Fruit", null, null, false, true, false, true, true),
                Pair.of("none", "Apple"), Pair.of("none", "Banana"));

        validator.validate(multiValuedInstance);
    }
}
