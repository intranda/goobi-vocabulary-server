package io.goobi.vocabularyserver.repositories;

import io.goobi.vocabularyserver.model.Vocabulary;
import org.springframework.data.repository.CrudRepository;
import org.springframework.lang.NonNull;

import java.util.Optional;

public interface VocabularyRepository extends CrudRepository<Vocabulary, Integer> {
    Optional<Vocabulary> findByName(@NonNull String name);
}