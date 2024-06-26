package io.goobi.vocabulary.service.io.excel;

import io.goobi.vocabulary.model.jpa.VocabularyEntity;

import java.io.OutputStream;

public interface ExcelMapper {
    void toExcel(VocabularyEntity entity, OutputStream out);
}
