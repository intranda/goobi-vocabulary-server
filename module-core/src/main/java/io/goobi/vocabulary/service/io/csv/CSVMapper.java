package io.goobi.vocabulary.service.io.csv;

import io.goobi.vocabulary.model.jpa.VocabularyEntity;

public interface CSVMapper {
    String toCSV(VocabularyEntity entity);
}
