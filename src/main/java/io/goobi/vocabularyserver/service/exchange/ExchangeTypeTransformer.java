package io.goobi.vocabularyserver.service.exchange;

import io.goobi.vocabularyserver.exchange.FieldDefinitionDTO;
import io.goobi.vocabularyserver.exchange.FieldInstanceDTO;
import io.goobi.vocabularyserver.exchange.FieldTypeDTO;
import io.goobi.vocabularyserver.exchange.VocabularyDTO;
import io.goobi.vocabularyserver.exchange.VocabularyRecordDTO;
import io.goobi.vocabularyserver.exchange.VocabularySchemaDTO;
import io.goobi.vocabularyserver.model.FieldDefinition;
import io.goobi.vocabularyserver.model.FieldInstance;
import io.goobi.vocabularyserver.model.FieldType;
import io.goobi.vocabularyserver.model.Vocabulary;
import io.goobi.vocabularyserver.model.VocabularyRecord;
import io.goobi.vocabularyserver.model.VocabularySchema;

public interface ExchangeTypeTransformer {
    FieldDefinitionDTO transform(FieldDefinition fieldDefinition);

    FieldTypeDTO transform(FieldType fieldType);

    VocabularyDTO transform(Vocabulary vocabulary);

    VocabularySchemaDTO transform(VocabularySchema vocabularySchema);

    VocabularyRecordDTO transform(VocabularyRecord vocabularyRecord);

    FieldInstanceDTO transform(FieldInstance fieldInstance);
}
