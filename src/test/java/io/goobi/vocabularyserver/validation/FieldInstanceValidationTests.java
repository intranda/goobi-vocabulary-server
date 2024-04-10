package io.goobi.vocabularyserver.validation;

import io.goobi.vocabularyserver.exception.ValidationException;
import io.goobi.vocabularyserver.model.FieldDefinitionEntity;
import io.goobi.vocabularyserver.model.FieldInstanceEntity;
import io.goobi.vocabularyserver.model.FieldTranslationEntity;
import io.goobi.vocabularyserver.model.FieldTypeEntity;
import io.goobi.vocabularyserver.model.FieldValueEntity;
import io.goobi.vocabularyserver.model.LanguageEntity;
import io.goobi.vocabularyserver.model.SelectableValueEntity;
import io.goobi.vocabularyserver.model.VocabularyEntity;
import io.goobi.vocabularyserver.model.VocabularyRecordEntity;
import io.goobi.vocabularyserver.model.VocabularySchemaEntity;
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

    private VocabularySchemaEntity schema;
    private VocabularyRecordEntity record;


    @BeforeEach
    public void setUp() {
        schema = new VocabularySchemaEntity();
        VocabularyEntity vocabulary = new VocabularyEntity();
        vocabulary.setSchema(schema);
        vocabulary.setName("Test vocabulary");
        record = new VocabularyRecordEntity();
        record.setVocabulary(vocabulary);
    }

    private FieldDefinitionEntity setupFieldDefinition(String name, String validation, List<String> selectableValues, boolean mainEntry, boolean titleField, boolean unique, boolean required, boolean multiValued) {
        FieldTypeEntity type = new FieldTypeEntity();
        type.setValidation(validation);
        if (selectableValues != null) {
            type.setSelectableValues(selectableValues.stream().map(s -> {
                SelectableValueEntity sv = new SelectableValueEntity();
                sv.setValue(s);
                return sv;
            }).collect(Collectors.toList()));
        }
        FieldDefinitionEntity definition = new FieldDefinitionEntity();
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

    private FieldInstanceEntity setupFieldInstance(FieldDefinitionEntity definition, Pair<String, String>... values) {
        FieldInstanceEntity field = new FieldInstanceEntity();
        field.setDefinition(definition);
        field.setVocabularyRecord(record);
        field.setFieldValues(transform(field, values));
        record.setFields(List.of(field));
        return field;
    }

    private List<FieldValueEntity> transform(FieldInstanceEntity field, Pair<String, String>[] values) {
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
                .collect(Collectors.toList());
    }

    private LanguageEntity createLanguage(String abbreviation) {
        LanguageEntity language = new LanguageEntity();
        language.setAbbreviation(abbreviation);
        return language;
    }

    private FieldValueEntity createFieldValue(FieldInstanceEntity field, LanguageEntity language, String value) {
        FieldValueEntity fieldValue = new FieldValueEntity();
        fieldValue.setFieldInstance(field);
        fieldValue.setTranslations(List.of(createTranslation(fieldValue, language, value)));
        return fieldValue;
    }

    private FieldTranslationEntity createTranslation(FieldValueEntity field, LanguageEntity language, String value) {
        FieldTranslationEntity translation = new FieldTranslationEntity();
        translation.setFieldValue(field);
        translation.setLanguage(language);
        translation.setValue(value);
        return translation;
    }

    @Test
    void textFieldValueNotMatchingValidation_fails() {
        FieldInstanceEntity field = setupFieldInstance(
                setupFieldDefinition("name", "\\w+", null, true, true, true, true, false),
                Pair.of("none", "Thomas Lastname"));

        assertThrows(ValidationException.class, () -> validator.validate(field));
    }

    @Test
    void textFieldValueMatchingValidation_success() throws ValidationException {
        FieldInstanceEntity field = setupFieldInstance(
                setupFieldDefinition("name", "\\w+", null, true, true, true, true, false),
                Pair.of("none", "Thomas"));

        validator.validate(field);
    }

    @Test
    void numberFieldValueNotMatchingValidation_fails() {
        FieldInstanceEntity field = setupFieldInstance(
                setupFieldDefinition("age", "\\d+", null, true, true, true, true, false),
                Pair.of("none", "Thomas"));

        assertThrows(ValidationException.class, () -> validator.validate(field));
    }

    @Test
    void numberFieldValueMatchingValidation_fails() throws ValidationException {
        FieldInstanceEntity field = setupFieldInstance(
                setupFieldDefinition("age", "\\d+", null, true, true, true, true, false),
                Pair.of("none", "32"));

        validator.validate(field);
    }

    @Test
    void valueIsOneOfTheSelectableValues_success() throws ValidationException {
        FieldInstanceEntity field = setupFieldInstance(
                setupFieldDefinition("OS", null, List.of("Linux", "Windows"), false, false, false, false, false),
                Pair.of("none", "Linux"));

        validator.validate(field);
    }

    @Test
    void valueIsNotOneOfTheSelectableValues_success() {
        FieldInstanceEntity field = setupFieldInstance(
                setupFieldDefinition("OS", null, List.of("Linux", "Windows"), false, false, false, false, false),
                Pair.of("none", "MacOS"));

        assertThrows(ValidationException.class, () -> validator.validate(field));
    }

    @Test
    void emptyFieldValue_fails() {
        FieldInstanceEntity field = setupFieldInstance(
                setupFieldDefinition("hobbies", null, null, true, true, true, true, false),
                Pair.of("none", ""));

        assertThrows(ValidationException.class, () -> validator.validate(field));
    }

    @Test
    void duplicateUniqueFieldValueForAnotherFieldDefinition_success() throws ValidationException {
        when(fieldInstanceRepository.existsByVocabularyRecord_Vocabulary_IdAndDefinition_IdAndIdNotAndFieldValues_Translations_Value(record.getVocabulary().getId(), 1L, 11L, "Bob")).thenReturn(true);
        when(fieldInstanceRepository.existsByVocabularyRecord_Vocabulary_IdAndDefinition_IdAndIdNotAndFieldValues_Translations_Value(record.getVocabulary().getId(), 2L, 12L, "Bob")).thenReturn(false);

        FieldInstanceEntity fieldFriend = setupFieldInstance(
                setupFieldDefinition("Best Friend", null, null, false, true, false, true, false),
                Pair.of("none", "Thomas"));
        fieldFriend.getDefinition().setId(1L);
        fieldFriend.setId(11L);
        FieldInstanceEntity fieldName = setupFieldInstance(
                setupFieldDefinition("Name", null, null, true, true, true, true, false),
                Pair.of("none", "Bob"));
        fieldName.getDefinition().setId(2L);
        fieldName.setId(12L);

        validator.validate(fieldFriend);
        validator.validate(fieldName);
    }

    @Test
    void duplicateUniqueFieldValueForTheSameFieldDefinition_fails() {
        when(fieldInstanceRepository.existsByVocabularyRecord_Vocabulary_IdAndDefinition_IdAndIdNotAndFieldValues_Translations_Value(record.getVocabulary().getId(), 2L, 10L, "Bob")).thenReturn(true);

        FieldInstanceEntity fieldName = setupFieldInstance(
                setupFieldDefinition("Name", null, null, true, true, true, true, false),
                Pair.of("none", "Bob"));
        fieldName.getDefinition().setId(2L);
        fieldName.setId(10L);

        assertThrows(ValidationException.class, () -> validator.validate(fieldName));
    }

    @Test
    void multipleFieldValuesIfMultiValuedIsDisabled_fails() {
        FieldInstanceEntity multiValuedInstance = setupFieldInstance(
                setupFieldDefinition("Fruit", null, null, false, true, false, true, false),
                Pair.of("none", "Apple"), Pair.of("none", "Banana"));

        assertThrows(ValidationException.class, () -> validator.validate(multiValuedInstance));
    }

    @Test
    void multipleFieldValuesIfMultiValuedIsEnabled_success() throws ValidationException {
        FieldInstanceEntity multiValuedInstance = setupFieldInstance(
                setupFieldDefinition("Fruit", null, null, false, true, false, true, true),
                Pair.of("none", "Apple"), Pair.of("none", "Banana"));

        validator.validate(multiValuedInstance);
    }
}
