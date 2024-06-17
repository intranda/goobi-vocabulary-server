package io.goobi.vocabulary.service.manager;

import io.goobi.vocabulary.exception.EntityNotFoundException;
import io.goobi.vocabulary.exception.ValidationException;
import io.goobi.vocabulary.exchange.FieldInstance;
import io.goobi.vocabulary.exchange.FieldValue;
import io.goobi.vocabulary.exchange.TranslationInstance;
import io.goobi.vocabulary.exchange.VocabularyRecord;
import io.goobi.vocabulary.model.jpa.FieldDefinitionEntity;
import io.goobi.vocabulary.model.jpa.LanguageEntity;
import io.goobi.vocabulary.model.jpa.TranslationDefinitionEntity;
import io.goobi.vocabulary.model.jpa.VocabularyEntity;
import io.goobi.vocabulary.repositories.VocabularyRepository;
import jakarta.transaction.Transactional;
import lombok.AllArgsConstructor;
import lombok.Data;
import org.springframework.stereotype.Service;

import java.util.Arrays;
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
public class VocabularyImportManager {
    public static final String CSV_DELIMITER = ",";

    @Data
    @AllArgsConstructor
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

    private final VocabularyRepository vocabularyRepository;
    private final RecordDTOManager manager;
    private VocabularyEntity vocabulary;
    private long vocabularyId;
    private List<FieldInformation> fields = Collections.emptyList();
    private Pattern languagePattern = Pattern.compile("^.*( \\(\\w{3}\\))$");

    public VocabularyImportManager(VocabularyRepository vocabularyRepository, RecordDTOManager manager) {
        // TODO: Check if between request persistence is an issue
        this.vocabularyRepository = vocabularyRepository;
        this.manager = manager;
    }

    @Transactional
    public void importCsv(long vocabularyId, String csv) {
        this.vocabularyId = vocabularyId;
        this.vocabulary = vocabularyRepository.findById(vocabularyId)
                .orElseThrow(() -> new EntityNotFoundException(VocabularyEntity.class, vocabularyId));

        List<String> lines = extractLines(csv);
        String header = lines.get(0);
        parseHeader(header);
        fields.forEach(System.err::println);
        validateHeader();
        List<Throwable> importErrors = new LinkedList<>();

        // parse all records and create a queue
        List<VocabularyRecord> insertionQueue = new LinkedList<>();
        for (int i = 1; i < lines.size(); i++) {
            try {
                insertionQueue.add(parseRecord(lines.get(i)));
            } catch (IllegalArgumentException e) {
                importErrors.add(e);
            }
        }

        // insert all records in this queue, if the parent has been already inserted, otherwise enqueue last
        Map<Long, Long> idMapping = new HashMap<>();
        // TODO: Might lead to infinite loop on ID or import errors
        while (!insertionQueue.isEmpty()) {
            try {
                VocabularyRecord newRecord = insertionQueue.get(0);
                insertionQueue.remove(0);
                Long oldId = newRecord.getId();
                Long parentId = newRecord.getParentId();

                // if parent is not yet processed, enqueue last
                if (parentId != null && !idMapping.containsKey(parentId)) {
                    throw new ValidationException("Undefined parent \"" + parentId + "\"");
                }

                if (parentId != null) {
                    newRecord.setParentId(idMapping.get(parentId));
                    if (oldId != null && manager.exists(oldId)) {
                        newRecord = replaceExistingRecordsValues(newRecord, oldId);
                        newRecord = manager.replace(newRecord);
                    } else {
                        newRecord.setId(null);
                        newRecord = manager.createSubRecord(newRecord);
                    }
                } else {
                    if (oldId != null && manager.exists(oldId)) {
                        newRecord = replaceExistingRecordsValues(newRecord, oldId);
                        newRecord = manager.replace(newRecord);
                    } else {
                        newRecord.setId(null);
                        newRecord = manager.create(newRecord);
                    }
                }
                long newId = newRecord.getId();
                if (oldId != null) {
                    idMapping.put(oldId, newId);
                }
            } catch (ValidationException e) {
                importErrors.add(e);
            }
        }
        if (!importErrors.isEmpty()) {
            throw new IllegalArgumentException("Error(s) during csv import:\n\t"
                    + importErrors.stream().map(Throwable::getMessage).collect(Collectors.joining("\n\t")));
        }
    }

    private VocabularyRecord replaceExistingRecordsValues(VocabularyRecord newRecord, long oldId) {
        VocabularyRecord existingRecord = manager.get(oldId);
        existingRecord.setParentId(newRecord.getParentId());
        existingRecord.setFields(newRecord.getFields());
        existingRecord.getFields().forEach(f -> f.setRecordId(oldId));
        return existingRecord;
    }

    private void parseHeader(String header) {
        fields = Arrays.stream(header.split(CSV_DELIMITER))
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
        Set<FieldInformation> presentFields = fields.stream().collect(Collectors.toSet());
        if (!definedFields.equals(presentFields)) {
            Set<FieldInformation> undefined = presentFields.stream()
                    .filter(f -> !definedFields.contains(f))
                    .collect(Collectors.toSet());
            Set<FieldInformation> missing = definedFields.stream()
                    .filter(f -> !presentFields.contains(f))
                    .collect(Collectors.toSet());
            String error = "Given fields do not match vocabulary schema:";
            if (!undefined.isEmpty()) {
                error += "\n\tUndefined fields: " + undefined.stream().map(FieldInformation::toString).collect(Collectors.joining(", "));
            }
            if (!missing.isEmpty()) {
                error += "\n\tMissing fields: " + missing.stream().map(FieldInformation::toString).collect(Collectors.joining(", "));
            }
            throw new IllegalArgumentException(error);
        }
    }

    private VocabularyRecord parseRecord(String rec) {
        VocabularyRecord resultRecord = new VocabularyRecord();

        String[] values = rec.split(CSV_DELIMITER, -1);
        if (values.length != fields.size()) {
            throw new IllegalArgumentException("Malformed CSV, number of fields does not match header");
        }

        Map<String, FieldInstance> fieldMap = new HashMap<>();
        for (int i = 0; i < fields.size(); i++) {
            FieldInformation info = fields.get(i);
            String multiValue = values[i];
            if (multiValue.isBlank()) {
                continue;
            }
            // Special treatment for ID's
            if ("ID".equals(info.name)) {
                resultRecord.setId(Long.parseLong(multiValue));
            } else if ("Parent-ID".equals(info.name)) {
                resultRecord.setParentId(Long.parseLong(multiValue));
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

        resultRecord.setVocabularyId(vocabularyId);
        resultRecord.setFields(new HashSet<>(fieldMap.values()));

        return resultRecord;
    }

    private List<String> extractLines(String csv) {
        // TODO: Check performance on very large csv files
        return Arrays.stream(csv.split("\\n"))
                .map(String::strip)
                .filter(l -> !l.isBlank())
                .collect(Collectors.toList());
    }
}
