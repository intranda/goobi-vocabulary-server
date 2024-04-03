package io.goobi.vocabularyserver.repositories;

import io.goobi.vocabularyserver.model.Vocabulary;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.repository.ListCrudRepository;
import org.springframework.lang.NonNull;

import java.util.Optional;

public interface VocabularyRepository extends ListCrudRepository<Vocabulary, Long> {
    Page<Vocabulary> findAll(Pageable pageable);
    Optional<Vocabulary> findByName(@NonNull String name);
}