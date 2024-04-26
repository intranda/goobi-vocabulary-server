package io.goobi.vocabulary.service.manager;

import com.google.gson.Gson;
import io.goobi.vocabulary.exception.EntityNotFoundException;
import io.goobi.vocabulary.model.flat.Definition;
import io.goobi.vocabulary.model.flat.Field;
import io.goobi.vocabulary.model.flat.LanguageSpecification;
import io.goobi.vocabulary.model.flat.Record;
import io.goobi.vocabulary.model.flat.Schema;
import io.goobi.vocabulary.model.flat.Type;
import io.goobi.vocabulary.model.flat.Value;
import io.goobi.vocabulary.model.flat.Vocabulary;
import io.goobi.vocabulary.model.jpa.FieldDefinitionEntity;
import io.goobi.vocabulary.model.jpa.FieldInstanceEntity;
import io.goobi.vocabulary.model.jpa.FieldTranslationEntity;
import io.goobi.vocabulary.model.jpa.FieldTypeEntity;
import io.goobi.vocabulary.model.jpa.FieldValueEntity;
import io.goobi.vocabulary.model.jpa.SelectableValueEntity;
import io.goobi.vocabulary.model.jpa.TranslationDefinitionEntity;
import io.goobi.vocabulary.model.jpa.VocabularyEntity;
import io.goobi.vocabulary.model.jpa.VocabularyRecordEntity;
import io.goobi.vocabulary.model.jpa.VocabularySchemaEntity;
import io.goobi.vocabulary.repositories.VocabularyRepository;
import org.springframework.stereotype.Service;

import java.util.Collection;
import java.util.List;
import java.util.stream.Collectors;

@Service
public class VocabularyExportManager {
    private final VocabularyRepository vocabularyRepository;
    private final Gson gson;

    public VocabularyExportManager(VocabularyRepository vocabularyRepository) {
        this.vocabularyRepository = vocabularyRepository;
        this.gson = new Gson();
    }

    public String export(long vocabularyId) {
        return gson.toJson(transform(vocabularyRepository.findById(vocabularyId)
                .orElseThrow(() -> new EntityNotFoundException(VocabularyEntity.class, vocabularyId))));
    }

    private Vocabulary transform(VocabularyEntity original) {
        Vocabulary result = new Vocabulary();
        result.setId(original.getId());
        result.setName(original.getName());
        result.setDescription(original.getDescription());
        result.setSchema(transform(original.getSchema()));
        if (original.getRecords() != null && !original.getRecords().isEmpty()) {
            result.setEntries(original.getRecords().stream()
                    .map(this::transform)
                    .collect(Collectors.toList()));
        }
        return result;
    }

    private Schema transform(VocabularySchemaEntity original) {
        Schema result = new Schema();
        result.setId(original.getId());
        result.setHierarchicalRecordsEnabled(original.isHierarchicalRecords());
        result.setDefinitions(original.getDefinitions().stream()
                .map(this::transform)
                .collect(Collectors.toList()));
        return result;
    }

    private Definition transform(FieldDefinitionEntity original) {
        Definition result = new Definition();
        result.setId(original.getId());
        result.setLabel(original.getName());
        result.setType(transform(original.getType()));
        if (original.getTranslationDefinitions() != null && !original.getTranslationDefinitions().isEmpty()) {
            result.setLanguageSpecifications(original.getTranslationDefinitions().stream()
                    .map(this::transform)
                    .collect(Collectors.toList()));
        }
        result.setRequired(original.isRequired());
        result.setUnique(original.isUnique());
        result.setMainEntry(Boolean.TRUE.equals(original.getMainEntry()));
        result.setTitleField(original.isTitleField());
        result.setMultiValued(original.isMultiValued());
        return result;
    }

    private Type transform(FieldTypeEntity original) {
        Type result = new Type();
        result.setId(original.getId());
        result.setName(original.getName());
        if (original.getSelectableValues() != null && !original.getSelectableValues().isEmpty()) {
            result.setSelectableValues(original.getSelectableValues().stream()
                    .map(SelectableValueEntity::getValue)
                    .collect(Collectors.toList()));
        }
        return result;
    }

    private LanguageSpecification transform(TranslationDefinitionEntity original) {
        LanguageSpecification result = new LanguageSpecification();
        result.setId(original.getId());
        result.setLanguage(original.getLanguage().getName());
        result.setFallback(Boolean.TRUE.equals(original.getFallback()));
        result.setRequired(original.isRequired());
        return result;
    }

    private Record transform(VocabularyRecordEntity original) {
        Record result = new Record();
        result.setId(original.getId());
        if (original.getChildren() != null && !original.getChildren().isEmpty()) {
            result.setChildren(original.getChildren().stream()
                    .map(this::transform)
                    .collect(Collectors.toList()));
        }
        result.setFields(original.getFields().stream()
                .map(this::transform)
                .collect(Collectors.toList()));
        return result;
    }

    private Field transform(FieldInstanceEntity original) {
        Field result = new Field();
        result.setId(original.getId());
        result.setDefinitionId(original.getDefinition().getId());
        result.setValues(original.getFieldValues().stream()
                .map(this::transform)
                .flatMap(Collection::stream)
                .collect(Collectors.toList()));
        return result;
    }

    private List<Value> transform(FieldValueEntity original) {
        return original.getTranslations().stream()
                .map(this::transform)
                .collect(Collectors.toList());
    }

    private Value transform(FieldTranslationEntity original) {
        Value result = new Value();
        result.setValue(original.getValue());
        if (original.getLanguage() != null) {
            result.setLanguage(original.getLanguage().getName());
        }
        return result;
    }
}
