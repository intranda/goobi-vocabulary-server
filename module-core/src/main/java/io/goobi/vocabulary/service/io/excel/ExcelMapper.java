package io.goobi.vocabulary.service.io.excel;

import io.goobi.vocabulary.exchange.VocabularyRecord;
import io.goobi.vocabulary.model.jpa.VocabularyEntity;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.List;

public interface ExcelMapper {
    void toExcel(VocabularyEntity vocabulary, OutputStream out) throws IOException;

    List<VocabularyRecord> fromExcel(VocabularyEntity vocabulary, InputStream in) throws IOException;
}
