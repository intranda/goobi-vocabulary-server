package io.goobi.vocabularyserver.repositories;

import io.goobi.vocabularyserver.model.VocabularySchema;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.repository.ListCrudRepository;

public interface VocabularySchemaRepository extends ListCrudRepository<VocabularySchema, Long> {
    Page<VocabularySchema> findAll(Pageable pageable);
}