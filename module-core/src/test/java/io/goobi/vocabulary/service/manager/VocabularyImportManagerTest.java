package io.goobi.vocabulary.service.manager;

import io.goobi.vocabulary.exception.VocabularyException;
import io.goobi.vocabulary.exchange.VocabularyRecord;
import io.goobi.vocabulary.model.jpa.VocabularyEntity;
import io.goobi.vocabulary.repositories.VocabularyRepository;
import io.goobi.vocabulary.service.io.csv.CSVMapper;
import io.goobi.vocabulary.service.io.excel.ExcelMapper;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.function.Executable;
import org.mockito.ArgumentCaptor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Optional;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyLong;
import static org.mockito.Mockito.lenient;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
class VocabularyImportManagerTest {
    private static final long VOCABULARY_ID = 3L;
    private static final long OWN_RECORD_ID = 5L;
    private static final long FOREIGN_RECORD_ID = 7L;
    private static final long OTHER_FOREIGN_RECORD_ID = 9L;
    private static final String CSV = "csv content is irrelevant, the mapper is mocked";

    @Mock
    private VocabularyRepository vocabularyRepository;
    @Mock
    private RecordDTOManager manager;
    @Mock
    private CSVMapper csvMapper;
    @Mock
    private ExcelMapper excelMapper;
    @InjectMocks
    private VocabularyImportManager importManager;

    private final VocabularyEntity vocabulary = new VocabularyEntity();
    private final InputStream excel = new ByteArrayInputStream(new byte[0]);

    @BeforeEach
    void setUp() {
        vocabulary.setId(VOCABULARY_ID);
        when(vocabularyRepository.findById(VOCABULARY_ID)).thenReturn(Optional.of(vocabulary));
    }

    private static VocabularyRecord row(Long id) {
        VocabularyRecord result = new VocabularyRecord();
        result.setId(id);
        result.setFields(new HashSet<>());
        return result;
    }

    // The import manager consumes the queue, so each call gets a fresh mutable list
    private static List<VocabularyRecord> rows(VocabularyRecord... rows) {
        return new LinkedList<>(List.of(rows));
    }

    private void givenForeignRecords(long... ids) {
        // Let a wrongful write go through, so a missing check fails on the assertions, not on a null
        lenient().when(manager.create(any())).thenReturn(row(100L));
        for (long id : ids) {
            when(manager.existsInOtherVocabulary(id, VOCABULARY_ID)).thenReturn(true);
        }
    }

    private void assertRejectedForForeignIds(Executable importCall, String expectedIds) {
        VocabularyException e = assertThrows(VocabularyException.class, importCall);

        assertEquals(VocabularyException.ErrorCode.RecordImport, e.getErrorType());
        assertEquals(Map.of("recordIds", expectedIds), e.getParams());
        verify(manager, never()).deleteAllRecords(anyLong());
        verify(manager, never()).create(any());
        verify(manager, never()).createSubRecord(any());
        verify(manager, never()).replace(any());
    }

    @Test
    void importCsv_rowIdsFromAnotherVocabulary_rejectsWholeImportListingThoseIds() {
        when(csvMapper.fromCSV(vocabulary, CSV))
                .thenReturn(rows(row(FOREIGN_RECORD_ID), row(null), row(OTHER_FOREIGN_RECORD_ID)));
        givenForeignRecords(FOREIGN_RECORD_ID, OTHER_FOREIGN_RECORD_ID);

        assertRejectedForForeignIds(() -> importManager.importCsv(VOCABULARY_ID, CSV), "7, 9");
    }

    @Test
    void cleanImportCsv_rowIdFromAnotherVocabulary_rejectedBeforeDeletingExistingRecords() {
        when(csvMapper.fromCSV(vocabulary, CSV)).thenReturn(rows(row(FOREIGN_RECORD_ID)));
        givenForeignRecords(FOREIGN_RECORD_ID);

        assertRejectedForForeignIds(() -> importManager.cleanImportCsv(VOCABULARY_ID, CSV), "7");
    }

    @Test
    void importExcel_rowIdFromAnotherVocabulary_rejectsWholeImport() throws Exception {
        when(excelMapper.fromExcel(vocabulary, excel)).thenReturn(rows(row(FOREIGN_RECORD_ID)));
        givenForeignRecords(FOREIGN_RECORD_ID);

        assertRejectedForForeignIds(() -> importManager.importExcel(VOCABULARY_ID, excel), "7");
    }

    @Test
    void cleanImportExcel_rowIdFromAnotherVocabulary_rejectedBeforeDeletingExistingRecords() throws Exception {
        when(excelMapper.fromExcel(vocabulary, excel)).thenReturn(rows(row(FOREIGN_RECORD_ID)));
        givenForeignRecords(FOREIGN_RECORD_ID);

        assertRejectedForForeignIds(() -> importManager.cleanImportExcel(VOCABULARY_ID, excel), "7");
    }

    @Test
    void importCsv_rowIdFromSameVocabulary_replacesThatRecord() {
        when(csvMapper.fromCSV(vocabulary, CSV)).thenReturn(rows(row(OWN_RECORD_ID)));
        when(manager.exists(OWN_RECORD_ID)).thenReturn(true);
        when(manager.get(OWN_RECORD_ID)).thenReturn(row(OWN_RECORD_ID));
        when(manager.replace(any())).thenReturn(row(OWN_RECORD_ID));

        importManager.importCsv(VOCABULARY_ID, CSV);

        ArgumentCaptor<VocabularyRecord> replaced = ArgumentCaptor.forClass(VocabularyRecord.class);
        verify(manager).replace(replaced.capture());
        assertEquals(OWN_RECORD_ID, replaced.getValue().getId());
        verify(manager, never()).create(any());
    }

    @Test
    void importCsv_rowIdUnknownEverywhere_createsNewRecord() {
        when(csvMapper.fromCSV(vocabulary, CSV)).thenReturn(rows(row(42L)));
        when(manager.exists(42L)).thenReturn(false);
        when(manager.create(any())).thenReturn(row(100L));

        importManager.importCsv(VOCABULARY_ID, CSV);

        ArgumentCaptor<VocabularyRecord> created = ArgumentCaptor.forClass(VocabularyRecord.class);
        verify(manager).create(created.capture());
        assertNull(created.getValue().getId());
        verify(manager, never()).replace(any());
    }
}
