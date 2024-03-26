package io.goobi.vocabularyserver.repositories;

import io.goobi.vocabularyserver.model.FieldInstance;
import org.springframework.data.repository.CrudRepository;
import org.springframework.lang.NonNull;

public interface FieldInstanceRepository extends CrudRepository<FieldInstance, Long> {
    boolean existsByVocabularyRecord_Vocabulary_IdAndValue(@NonNull Long id, @NonNull String value);
}
