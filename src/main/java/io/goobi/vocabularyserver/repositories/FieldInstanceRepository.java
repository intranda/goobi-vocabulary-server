package io.goobi.vocabularyserver.repositories;

import io.goobi.vocabularyserver.model.FieldInstanceEntity;
import org.springframework.data.repository.CrudRepository;
import org.springframework.lang.NonNull;

public interface FieldInstanceRepository extends CrudRepository<FieldInstanceEntity, Long> {
    boolean existsByVocabularyRecord_Vocabulary_IdAndDefinition_IdAndIdNotAndFieldValues_Translations_Value(@NonNull Long id, @NonNull Long id1, @NonNull Long id2, @NonNull String value);
}
