package io.goobi.vocabulary.service.io.csv;

import io.goobi.vocabulary.exchange.VocabularyRecord;
import io.goobi.vocabulary.model.jpa.VocabularyEntity;
import io.goobi.vocabulary.service.io.TabularRecordExporter;
import io.goobi.vocabulary.service.io.TabularRecordImporter;
import org.springframework.stereotype.Service;

import java.util.Arrays;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

@Service
public class CSVMapperImpl implements CSVMapper {
    public static final String CSV_DELIMITER = ",";

    private final TabularRecordExporter tabularRecordExporter;
    private final TabularRecordImporter tabularRecordImporter;

    public CSVMapperImpl(TabularRecordExporter tabularRecordExporter, TabularRecordImporter tabularRecordImporter) {
        this.tabularRecordExporter = tabularRecordExporter;
        this.tabularRecordImporter = tabularRecordImporter;
    }

    @Override
    public String toCSV(VocabularyEntity entity) {
        List<List<String>> data = tabularRecordExporter.toTabularData(entity);
        return data.stream()
                .map(r -> String.join(CSV_DELIMITER, r))
                .collect(Collectors.joining("\n"));
    }

    @Override
    public List<VocabularyRecord> fromCSV(VocabularyEntity vocabulary, String csv) {
        List<List<String>> data = Arrays.stream(csv.split("\\n"))
                .map(line -> {
                    line = line.strip();
                    if (line.isBlank()) {
                        return null;
                    }
                    String[] values = line.split(CSV_DELIMITER, -1);
                    return List.of(values);
                })
                .filter(Objects::nonNull)
                .collect(Collectors.toList());
        return tabularRecordImporter.fromTabularData(vocabulary, data);
    }
}
