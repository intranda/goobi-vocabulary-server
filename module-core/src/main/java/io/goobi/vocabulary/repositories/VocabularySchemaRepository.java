package io.goobi.vocabulary.repositories;

import io.goobi.vocabulary.model.jpa.VocabularySchemaEntity;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.repository.ListCrudRepository;

public interface VocabularySchemaRepository extends ListCrudRepository<VocabularySchemaEntity, Long> {
    Page<VocabularySchemaEntity> findAll(Pageable pageable);
}