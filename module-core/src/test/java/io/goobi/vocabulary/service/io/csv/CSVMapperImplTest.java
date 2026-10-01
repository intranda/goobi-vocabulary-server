package io.goobi.vocabulary.service.io.csv;

import io.goobi.vocabulary.exception.VocabularyException;
import io.goobi.vocabulary.model.jpa.VocabularyEntity;
import io.goobi.vocabulary.service.io.TabularRecordExporter;
import io.goobi.vocabulary.service.io.TabularRecordImporter;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
class CSVMapperImplTest {
    private final VocabularyEntity vocabulary = new VocabularyEntity();

    @Mock
    private TabularRecordExporter tabularRecordExporter;
    @Mock
    private TabularRecordImporter tabularRecordImporter;
    @InjectMocks
    private CSVMapperImpl mapper;

    @Captor
    private ArgumentCaptor<List<List<String>>> importedData;

    private String export(List<List<String>> data) {
        when(tabularRecordExporter.toTabularData(vocabulary)).thenReturn(data);
        return mapper.toCSV(vocabulary);
    }

    private List<List<String>> importAndCapture(String csv) {
        mapper.fromCSV(vocabulary, csv);
        verify(tabularRecordImporter).fromTabularData(eq(vocabulary), importedData.capture());
        return importedData.getValue();
    }

    @Test
    void toCSV_plainValues_areWrittenUnquotedWithoutTrailingLineBreak() {
        String csv = export(List.of(
                List.of("ID", "Label (eng)", "Note"),
                List.of("1", "Role", ""),
                List.of("2", "Painter", "x|y")));

        assertEquals("ID,Label (eng),Note\n1,Role,\n2,Painter,x|y", csv);
    }

    @Test
    void toCSV_valueContainingComma_isQuoted() {
        String csv = export(List.of(
                List.of("ID", "Note"),
                List.of("1", "Rolle, die ein Akteur hat")));

        assertEquals("ID,Note\n1,\"Rolle, die ein Akteur hat\"", csv);
    }

    @Test
    void toCSV_valueContainingDoubleQuote_isQuotedWithQuoteDoubled() {
        String csv = export(List.of(
                List.of("ID", "Note"),
                List.of("1", "uses a \"punty\"")));

        assertEquals("ID,Note\n1,\"uses a \"\"punty\"\"\"", csv);
    }

    @Test
    void toCSV_valueContainingLineBreak_isQuoted() {
        String csv = export(List.of(
                List.of("ID", "Note"),
                List.of("1", "first line\nsecond line")));

        assertEquals("ID,Note\n1,\"first line\nsecond line\"", csv);
    }

    @Test
    void fromCSV_quotedValueContainingComma_staysOneField() {
        List<List<String>> data = importAndCapture("ID,Note\n1,\"Rolle, die ein Akteur hat\"");

        assertEquals(List.of(
                List.of("ID", "Note"),
                List.of("1", "Rolle, die ein Akteur hat")), data);
    }

    @Test
    void fromCSV_doubledQuoteInsideQuotedValue_isUnescaped() {
        List<List<String>> data = importAndCapture("ID,Note\n1,\"uses a \"\"punty\"\"\"");

        assertEquals(List.of(
                List.of("ID", "Note"),
                List.of("1", "uses a \"punty\"")), data);
    }

    @Test
    void fromCSV_quotedValueSpanningLines_staysOneRecord() {
        List<List<String>> data = importAndCapture("ID,Note\n1,\"first line\nsecond line\"\n2,plain");

        assertEquals(List.of(
                List.of("ID", "Note"),
                List.of("1", "first line\nsecond line"),
                List.of("2", "plain")), data);
    }

    @Test
    void fromCSV_crlfLineEndingsAndBlankLines_areAccepted() {
        List<List<String>> data = importAndCapture("ID,Label\r\n1,Role\r\n\r\n2,Painter\r\n");

        assertEquals(List.of(
                List.of("ID", "Label"),
                List.of("1", "Role"),
                List.of("2", "Painter")), data);
    }

    @Test
    void fromCSV_trailingEmptyFields_areKept() {
        List<List<String>> data = importAndCapture("ID,Label,Note,Related\n1,Role,,");

        assertEquals(List.of(
                List.of("ID", "Label", "Note", "Related"),
                List.of("1", "Role", "", "")), data);
    }

    @Test
    void fromCSV_unterminatedQuote_isRejectedAsRecordImportError() {
        VocabularyException e = assertThrows(VocabularyException.class,
                () -> mapper.fromCSV(vocabulary, "ID,Note\n1,\"never closed"));

        assertEquals(VocabularyException.ErrorCode.RecordImport, e.getErrorType());
        verifyNoInteractions(tabularRecordImporter);
    }

    @Test
    void exportThenImport_returnsTheOriginalRows() {
        List<List<String>> rows = List.of(
                List.of("ID", "Parent-ID", "Label (eng)", "Definition (eng)"),
                List.of("1", "", "Role", "Rolle, die ein Akteur hat"),
                List.of("2", "1", "Glass-blower", "uses a \"punty\",\nthen cools it"));

        String csv = export(rows);
        List<List<String>> data = importAndCapture(csv);

        assertEquals(rows, data);
    }
}
