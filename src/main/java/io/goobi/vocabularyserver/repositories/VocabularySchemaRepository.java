package io.goobi.vocabularyserver.repositories;

import io.goobi.vocabularyserver.model.VocabularySchema;
import org.springframework.data.repository.CrudRepository;

public interface VocabularySchemaRepository extends CrudRepository<VocabularySchema, Long> {
}