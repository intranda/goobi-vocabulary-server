package io.goobi.vocabulary.service.manager;

import io.goobi.vocabulary.exception.EntityNotFoundException;
import io.goobi.vocabulary.exception.ValidationException;
import io.goobi.vocabulary.exchange.VocabularyRecord;
import io.goobi.vocabulary.model.jpa.VocabularyEntity;
import io.goobi.vocabulary.repositories.VocabularyRepository;
import io.goobi.vocabulary.service.io.csv.CSVMapper;
import io.goobi.vocabulary.service.io.excel.ExcelMapper;
import jakarta.transaction.Transactional;
import org.springframework.stereotype.Service;

import java.io.IOException;
import java.io.InputStream;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

@Service
public class VocabularyImportManager {
    private final VocabularyRepository vocabularyRepository;
    private final RecordDTOManager manager;
    private final CSVMapper csvMapper;
    private final ExcelMapper excelMapper;
    private VocabularyEntity vocabulary;

    public VocabularyImportManager(VocabularyRepository vocabularyRepository, RecordDTOManager manager, CSVMapper csvMapper, ExcelMapper excelMapper) {
        // TODO: Check if between request persistence is an issue
        this.vocabularyRepository = vocabularyRepository;
        this.manager = manager;
        this.csvMapper = csvMapper;
        this.excelMapper = excelMapper;
    }

    @Transactional
    public void cleanImportCsv(long vocabularyId, String csv) {
        this.vocabulary = vocabularyRepository.findById(vocabularyId)
                .orElseThrow(() -> new EntityNotFoundException(VocabularyEntity.class, vocabularyId));

        manager.deleteAllRecords(vocabularyId);
        List<VocabularyRecord> insertionQueue = csvMapper.fromCSV(vocabulary, csv);

        performRecordImport(insertionQueue);
    }

    @Transactional
    public void importCsv(long vocabularyId, String csv) {
        this.vocabulary = vocabularyRepository.findById(vocabularyId)
                .orElseThrow(() -> new EntityNotFoundException(VocabularyEntity.class, vocabularyId));

        List<VocabularyRecord> insertionQueue = csvMapper.fromCSV(vocabulary, csv);

        performRecordImport(insertionQueue);
    }

    @Transactional
    public void cleanImportExcel(long vocabularyId, InputStream excel) throws IOException {
        this.vocabulary = vocabularyRepository.findById(vocabularyId)
                .orElseThrow(() -> new EntityNotFoundException(VocabularyEntity.class, vocabularyId));

        manager.deleteAllRecords(vocabularyId);
        List<VocabularyRecord> insertionQueue = excelMapper.fromExcel(vocabulary, excel);

        performRecordImport(insertionQueue);
    }

    @Transactional
    public void importExcel(long vocabularyId, InputStream excel) throws IOException {
        this.vocabulary = vocabularyRepository.findById(vocabularyId)
                .orElseThrow(() -> new EntityNotFoundException(VocabularyEntity.class, vocabularyId));

        List<VocabularyRecord> insertionQueue = excelMapper.fromExcel(vocabulary, excel);

        performRecordImport(insertionQueue);
    }

    private void performRecordImport(List<VocabularyRecord> insertionQueue) {
        List<Throwable> importErrors = new LinkedList<>();
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
            throw new IllegalArgumentException("Error(s) during tabular import:\n\t"
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
}
