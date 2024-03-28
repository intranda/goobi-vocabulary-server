package io.goobi.vocabularyserver.repositories;

import io.goobi.vocabularyserver.model.FieldInstance;
import org.springframework.data.repository.CrudRepository;
import org.springframework.lang.NonNull;

public interface FieldInstanceRepository extends CrudRepository<FieldInstance, Long> {
//    boolean existsByVocabularyRecord_Vocabulary_IdAndDefinition_IdAndValue(@NonNull Long id, @NonNull Long id1, @NonNull String value);
}
