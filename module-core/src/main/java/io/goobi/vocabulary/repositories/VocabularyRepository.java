package io.goobi.vocabulary.repositories;

import io.goobi.vocabulary.model.jpa.VocabularyEntity;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.repository.ListCrudRepository;
import org.springframework.lang.NonNull;

import java.util.Optional;
import java.util.Set;

public interface VocabularyRepository extends ListCrudRepository<VocabularyEntity, Long> {
    Page<VocabularyEntity> findAll(Pageable pageable);
    Optional<VocabularyEntity> findByName(@NonNull String name);

    Set<VocabularyEntity> findDistinctBySchema_Definitions_ReferenceVocabulary(@NonNull VocabularyEntity referenceVocabulary);
}