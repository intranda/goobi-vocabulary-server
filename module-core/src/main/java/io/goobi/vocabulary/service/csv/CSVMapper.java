package io.goobi.vocabulary.service.csv;

import io.goobi.vocabulary.model.jpa.VocabularyEntity;

public interface CSVMapper {
    String toCSV(VocabularyEntity entity);
}
