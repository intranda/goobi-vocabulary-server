package io.goobi.vocabulary.repositories;

import io.goobi.vocabulary.model.VocabularyEntity;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.repository.ListCrudRepository;
import org.springframework.lang.NonNull;

import java.util.Optional;

public interface VocabularyRepository extends ListCrudRepository<VocabularyEntity, Long> {
    Page<VocabularyEntity> findAll(Pageable pageable);
    Optional<VocabularyEntity> findByName(@NonNull String name);
}