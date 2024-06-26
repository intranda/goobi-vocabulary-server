package io.goobi.vocabulary.service.io.csv;

import io.goobi.vocabulary.model.jpa.VocabularyEntity;
import io.goobi.vocabulary.service.io.TabularRecordExporter;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.stream.Collectors;

@Service
public class CSVMapperImpl implements CSVMapper {
    public static final String CSV_DELIMITER = ",";

    private final TabularRecordExporter tabularRecordExporter;

    public CSVMapperImpl(TabularRecordExporter tabularRecordExporter) {
        this.tabularRecordExporter = tabularRecordExporter;
    }

    @Override
    public String toCSV(VocabularyEntity entity) {
        List<List<String>> data = tabularRecordExporter.toTabularData(entity);
        return data.stream()
                .map(r -> String.join(CSV_DELIMITER, r))
                .collect(Collectors.joining("\n"));
    }
}
