package io.goobi.vocabulary.service.io.csv;

import io.goobi.vocabulary.exception.VocabularyException;
import io.goobi.vocabulary.exchange.VocabularyRecord;
import io.goobi.vocabulary.model.jpa.VocabularyEntity;
import io.goobi.vocabulary.service.io.TabularRecordExporter;
import io.goobi.vocabulary.service.io.TabularRecordImporter;
import org.apache.commons.csv.CSVFormat;
import org.apache.commons.csv.CSVParser;
import org.apache.commons.csv.CSVPrinter;
import org.apache.commons.csv.CSVRecord;
import org.springframework.stereotype.Service;

import java.io.IOException;
import java.io.StringWriter;
import java.io.UncheckedIOException;
import java.util.List;
import java.util.Map;

import static io.goobi.vocabulary.exception.VocabularyException.ErrorCode.RecordImport;

@Service
public class CSVMapperImpl implements CSVMapper {
    // RFC 4180 quoting, but with "\n" line breaks to keep plain exports unchanged
    private static final CSVFormat CSV_FORMAT = CSVFormat.DEFAULT.builder()
            .setRecordSeparator("\n")
            .get();

    private final TabularRecordExporter tabularRecordExporter;
    private final TabularRecordImporter tabularRecordImporter;

    public CSVMapperImpl(TabularRecordExporter tabularRecordExporter, TabularRecordImporter tabularRecordImporter) {
        this.tabularRecordExporter = tabularRecordExporter;
        this.tabularRecordImporter = tabularRecordImporter;
    }

    @Override
    public String toCSV(VocabularyEntity entity) {
        List<List<String>> data = tabularRecordExporter.toTabularData(entity);
        StringWriter out = new StringWriter();
        try (CSVPrinter printer = new CSVPrinter(out, CSV_FORMAT)) {
            // Separate records instead of terminating them, so there is no trailing line break
            for (int i = 0; i < data.size(); i++) {
                if (i > 0) {
                    printer.println();
                }
                for (String value : data.get(i)) {
                    printer.print(value);
                }
            }
        } catch (IOException e) {
            throw new UncheckedIOException(e);
        }
        return out.toString();
    }

    @Override
    public List<VocabularyRecord> fromCSV(VocabularyEntity vocabulary, String csv) {
        List<List<String>> data;
        try (CSVParser parser = CSVParser.parse(csv, CSV_FORMAT)) {
            data = parser.stream()
                    .map(CSVRecord::toList)
                    .toList();
        } catch (IOException | UncheckedIOException e) {
            throw new VocabularyException(RecordImport, null, Map.of(
                    "reason", String.valueOf(e.getMessage())
            ), (params) -> "Malformed CSV data: " + params.get("reason"));
        }
        return tabularRecordImporter.fromTabularData(vocabulary, data);
    }
}
