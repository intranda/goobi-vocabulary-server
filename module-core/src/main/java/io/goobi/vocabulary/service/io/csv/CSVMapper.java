package io.goobi.vocabulary.service.io.csv;

import io.goobi.vocabulary.exchange.VocabularyRecord;
import io.goobi.vocabulary.model.jpa.VocabularyEntity;

import java.util.List;

public interface CSVMapper {
    String toCSV(VocabularyEntity entity);
    List<VocabularyRecord> fromCSV(VocabularyEntity vocabulary, String csv);
}
