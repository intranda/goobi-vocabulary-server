package io.goobi.vocabulary.service.io.json;

import io.goobi.vocabulary.model.jpa.VocabularyEntity;

public interface JsonMapper {
    String toJson(VocabularyEntity vocabulary);
}
