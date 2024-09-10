package io.goobi.vocabulary.service.io;

import io.goobi.vocabulary.exception.VocabularyException;
import io.goobi.vocabulary.exchange.FieldInstance;
import io.goobi.vocabulary.exchange.FieldValue;
import io.goobi.vocabulary.exchange.TranslationInstance;
import io.goobi.vocabulary.exchange.VocabularyRecord;
import io.goobi.vocabulary.model.jpa.FieldDefinitionEntity;
import io.goobi.vocabulary.model.jpa.LanguageEntity;
import io.goobi.vocabulary.model.jpa.TranslationDefinitionEntity;
import io.goobi.vocabulary.model.jpa.VocabularyEntity;
import jakarta.transaction.Transactional;
import lombok.AllArgsConstructor;
import lombok.Data;
import org.springframework.stereotype.Service;

import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

@Service
public class TabularRecordImporter {
    @Data
    @AllArgsConstructor
    static
    class FieldInformation {
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

    private VocabularyEntity vocabulary;
    private List<FieldInformation> fields = Collections.emptyList();
    private final Pattern languagePattern = Pattern.compile("^.*( \\(\\w{3}\\))$");

    @Transactional
    public List<VocabularyRecord> fromTabularData(VocabularyEntity vocabulary, List<List<String>> data) {
        this.vocabulary = vocabulary;

        List<String> header = data.get(0);
        parseHeader(header);
        validateHeader();
        List<VocabularyException> importErrors = new LinkedList<>();

        // parse all records and create a queue
        List<VocabularyRecord> result = new LinkedList<>();
        for (int i = 1; i < data.size(); i++) {
            try {
                result.add(parseRecord(data.get(i)));
            } catch (VocabularyException e) {
                importErrors.add(e);
            }
        }

        if (!importErrors.isEmpty()) {
            throw new VocabularyException(VocabularyException.ErrorCode.RecordImportParsingIssues, importErrors, null,
                    params -> "Errors during record parsing");
        }

        return result;
    }

    private void parseHeader(List<String> header) {
        fields = header.stream()
                .map(this::parseField)
                .collect(Collectors.toList());
    }

    private FieldInformation parseField(String field) {
        // Check for existing language suffix
        Matcher m = languagePattern.matcher(field);
        String language = null;
        String name = field;
        if (m.find()) {
            String suffix = m.group(1);
            name = field.substring(0, field.indexOf(suffix));
            language = suffix.substring(2, 5);
        }
        return new FieldInformation(name, language);
    }

    private void validateHeader() {
        Set<FieldInformation> definedFields = new HashSet<>();
        definedFields.add(new FieldInformation("ID", null));
        if (vocabulary.getSchema().isHierarchicalRecords()) {
            definedFields.add(new FieldInformation("Parent-ID", null));
        }
        definedFields.addAll(vocabulary.getSchema().getDefinitions().stream()
                .flatMap(d -> {
                    String name = d.getName();
                    List<String> languages = d.getTranslationDefinitions().stream()
                            .map(TranslationDefinitionEntity::getLanguage)
                            .map(LanguageEntity::getAbbreviation)
                            .collect(Collectors.toList());
                    if (languages.isEmpty()) {
                        return List.of(new FieldInformation(name, null)).stream();
                    }
                    return languages.stream()
                            .map(l -> new FieldInformation(name, l));
                })
                .collect(Collectors.toSet()));
        Set<FieldInformation> presentFields = new HashSet<>(fields);
        if (!definedFields.equals(presentFields)) {
            Set<FieldInformation> undefined = presentFields.stream()
                    .filter(f -> !definedFields.contains(f))
                    .collect(Collectors.toSet());
            Set<FieldInformation> missing = definedFields.stream()
                    .filter(f -> !presentFields.contains(f))
                    .collect(Collectors.toSet());
            throw new VocabularyException(VocabularyException.ErrorCode.RecordImportHeaderIssues, null, Map.of(
                    "undefinedFields", undefined.stream().map(FieldInformation::toString).collect(Collectors.joining(",")),
                    "missingFields", missing.stream().map(FieldInformation::toString).collect(Collectors.joining(","))
            ),
                    params -> "Given fields do not match vocabulary schema, undefined fields \"" + params.get("undefinedFields") + "\", missing fields \"" + params.get("missingFields") + "\"");
        }
    }

    private VocabularyRecord parseRecord(List<String> rec) {
        VocabularyRecord resultRecord = new VocabularyRecord();

        if (rec.size() != fields.size()) {
            throw new VocabularyException(VocabularyException.ErrorCode.RecordImportFieldCountIssue, null, Map.of(
                    "expectedSize", String.valueOf(fields.size()),
                    "realSize", String.valueOf(rec.size())
            ),
                    params -> "Malformed tabular data, number of fields does not match header. Header size \"" + params.get("expectedSize") + "\", data row size \"" + params.get("realSize") + "\"");
        }

        Map<String, FieldInstance> fieldMap = new HashMap<>();
        for (int i = 0; i < fields.size(); i++) {
            FieldInformation info = fields.get(i);
            String multiValue = rec.get(i);
            if (multiValue == null || multiValue.isBlank()) {
                continue;
            }
            // Special treatment for ID's
            if ("ID".equals(info.name)) {
                resultRecord.setId(parseId(multiValue));
            } else if ("Parent-ID".equals(info.name)) {
                resultRecord.setParentId(parseId(multiValue));
            } else {
                FieldInstance field = fieldMap.computeIfAbsent(info.name, k -> {
                    FieldInstance result = new FieldInstance();
                    result.setDefinitionId(vocabulary.getSchema().getDefinitions().stream()
                            .filter(d -> d.getName().equals(k))
                            .map(FieldDefinitionEntity::getId)
                            .findFirst()
                            .orElseThrow());
                    return result;
                });
                // TODO: Test this with translatable multi-values
                String[] multiValues = multiValue.split("\\|");
                for (int j = 0; j < multiValues.length; j++) {
                    FieldValue fieldValue;
                    if (field.getValues().size() <= j) {
                        fieldValue = new FieldValue();
                        field.getValues().add(fieldValue);
                    } else {
                        fieldValue = field.getValues().get(j);
                    }
                    TranslationInstance translation = new TranslationInstance();
                    translation.setLanguage(info.getLanguage());
                    translation.setValue(multiValues[j]);
                    fieldValue.getTranslations().add(translation);
                }
            }
        }

        resultRecord.setVocabularyId(this.vocabulary.getId());
        resultRecord.setFields(new HashSet<>(fieldMap.values()));

        return resultRecord;
    }

    private static long parseId(String multiValue) {
        if (multiValue.contains(".")) {
            return (long) Double.parseDouble(multiValue);
        }
        return Long.parseLong(multiValue);
    }
}
