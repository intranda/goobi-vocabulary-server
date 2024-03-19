package io.goobi.vocabularyserver.repositories;

import io.goobi.vocabularyserver.model.Vocabulary;
import org.springframework.data.repository.ListCrudRepository;
import org.springframework.lang.NonNull;

import java.util.Optional;

public interface VocabularyRepository extends ListCrudRepository<Vocabulary, Long> {
    Optional<Vocabulary> findByName(@NonNull String name);
}