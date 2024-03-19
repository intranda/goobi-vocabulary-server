package io.goobi.vocabularyserver.service.exchange;

import io.goobi.vocabularyserver.exchange.FieldDefinition;
import io.goobi.vocabularyserver.exchange.FieldInstance;
import io.goobi.vocabularyserver.exchange.FieldType;
import io.goobi.vocabularyserver.exchange.Vocabulary;
import io.goobi.vocabularyserver.exchange.VocabularyRecord;
import io.goobi.vocabularyserver.exchange.VocabularySchema;

public interface ExchangeTypeTransformer {
    FieldDefinition transform(io.goobi.vocabularyserver.model.FieldDefinition fieldDefinition);

    FieldType transform(io.goobi.vocabularyserver.model.FieldType fieldType);

    Vocabulary transform(io.goobi.vocabularyserver.model.Vocabulary vocabulary);

    VocabularySchema transform(io.goobi.vocabularyserver.model.VocabularySchema vocabularySchema);

    VocabularyRecord transform(io.goobi.vocabularyserver.model.VocabularyRecord vocabularyRecord);

    FieldInstance transform(io.goobi.vocabularyserver.model.FieldInstance fieldInstance);
}
