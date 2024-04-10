package io.goobi.vocabularyserver.repositories;

import io.goobi.vocabularyserver.model.VocabularySchemaEntity;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.repository.ListCrudRepository;

public interface VocabularySchemaRepository extends ListCrudRepository<VocabularySchemaEntity, Long> {
    Page<VocabularySchemaEntity> findAll(Pageable pageable);
}