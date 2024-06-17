package io.goobi.vocabulary.service.csv;

import io.goobi.vocabulary.model.jpa.FieldDefinitionEntity;
import io.goobi.vocabulary.model.jpa.FieldInstanceEntity;
import io.goobi.vocabulary.model.jpa.FieldTranslationEntity;
import io.goobi.vocabulary.model.jpa.FieldValueEntity;
import io.goobi.vocabulary.model.jpa.LanguageEntity;
import io.goobi.vocabulary.model.jpa.TranslationDefinitionEntity;
import io.goobi.vocabulary.model.jpa.VocabularyEntity;
import io.goobi.vocabulary.model.jpa.VocabularyRecordEntity;
import lombok.AllArgsConstructor;
import lombok.Data;
import org.springframework.stereotype.Service;

import java.util.Collection;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

@Service
public class CSVMapperImpl implements CSVMapper {
    @Data
    @AllArgsConstructor
    class FieldDefinition {
        private String name;
        private String language;

        @Override
        public String toString() {
            if (language == null) {
                return name;
            }
            return name + " (" + language + ")";
        }
    }

    @Data
    @AllArgsConstructor
    class Field {
        private String name;
        private String language;
        private String value;
    }

    @Data
    class Row {
        private Set<Field> fields = new HashSet<>();

        public void addField(Field field) {
            fields.add(field);
        }

        public String getField(String name, String language) {
            return fields.stream()
                    .filter(f -> f.getName().equals(name))
                    .filter(f -> {
                        if (language != null) {
                            return language.equals(f.getLanguage());
                        } else {
                            return true;
                        }
                    })
                    .map(Field::getValue)
                    .findFirst()
                    .orElse("");
        }
    }

    @Override
    public String toCSV(VocabularyEntity entity) {
        StringBuilder sb = new StringBuilder();
        List<FieldDefinition> fieldDefinitions = createHeader(sb, entity);
        entity.getRecords().forEach(r -> dumpRecord(sb, fieldDefinitions, entity, r));
        return sb.toString();
    }

    private List<FieldDefinition> createHeader(StringBuilder sb, VocabularyEntity vocabulary) {
        List<FieldDefinition> fieldDefinitions = new LinkedList<>();
        fieldDefinitions.add(new FieldDefinition("ID", null));
        if (vocabulary.getSchema().isHierarchicalRecords()) {
            fieldDefinitions.add(new FieldDefinition("Parent-ID", null));
        }
        fieldDefinitions.addAll(vocabulary.getSchema().getDefinitions().stream()
                .sorted(Comparator.comparingLong(FieldDefinitionEntity::getId))
                .map(d -> {
                    if (d.getTranslationDefinitions() == null || d.getTranslationDefinitions().isEmpty()) {
                        return List.of(new FieldDefinition(d.getName(), null));
                    } else {
                        return d.getTranslationDefinitions().stream()
                                .map(TranslationDefinitionEntity::getLanguage)
                                .sorted(Comparator.comparingLong(LanguageEntity::getId))
                                .map(l -> new FieldDefinition(d.getName(), l.getAbbreviation()))
                                .collect(Collectors.toList());
                    }
                })
                .flatMap(Collection::stream)
                .collect(Collectors.toList()));
        sb.append(fieldDefinitions.stream()
                .map(FieldDefinition::toString)
                .collect(Collectors.joining(",")) + '\n');
        return fieldDefinitions;
    }

    private void dumpRecord(StringBuilder sb, List<FieldDefinition> fieldDefinitions, VocabularyEntity vocabulary, VocabularyRecordEntity r) {
        Row row = new Row();
        row.addField(new Field("ID", null, String.valueOf(r.getId())));
        if (vocabulary.getSchema().isHierarchicalRecords()) {
            row.addField(new Field("Parent-ID", null, r.getParentRecord() == null ? "" : String.valueOf(r.getParentRecord().getId())));
        }
        r.getFields().stream()
                .map(this::extractField)
                .flatMap(Collection::stream)
                .forEach(row::addField);
        sb.append(fieldDefinitions.stream()
                .map(fd -> row.getField(fd.name, fd.language))
                .collect(Collectors.joining(",")) + '\n');
    }

    private Set<Field> extractField(FieldInstanceEntity f) {
        Map<String, List<String>> languageValues = new HashMap<>();
        for (FieldValueEntity v : f.getFieldValues()) {
            for (FieldTranslationEntity t : v.getTranslations()) {
                String abbreviation = "";
                if (t.getLanguage() != null) {
                    abbreviation = t.getLanguage().getAbbreviation();
                }
                languageValues.computeIfAbsent(abbreviation, abb -> new LinkedList<>()).add(t.getValue());
            }
        }
        return languageValues.entrySet().stream()
                .map(e -> new Field(f.getDefinition().getName(), e.getKey().isBlank() ? null : e.getKey(), String.join("|", e.getValue())))
                .collect(Collectors.toSet());
    }
}
