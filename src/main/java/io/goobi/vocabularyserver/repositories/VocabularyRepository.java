package io.goobi.vocabularyserver.repositories;

import io.goobi.vocabularyserver.model.Vocabulary;
import org.springframework.data.repository.CrudRepository;

public interface VocabularyRepository extends CrudRepository<Vocabulary, Integer> {
}