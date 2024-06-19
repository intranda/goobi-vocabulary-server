package io.goobi.vocabulary.repositories;

import io.goobi.vocabulary.model.jpa.FieldInstanceEntity;
import org.springframework.data.repository.CrudRepository;
import org.springframework.lang.NonNull;

public interface FieldInstanceRepository extends CrudRepository<FieldInstanceEntity, Long> {
    boolean existsByVocabularyRecord_Vocabulary_IdAndDefinition_IdAndVocabularyRecord_IdNotAndFieldValues_Translations_Value(@NonNull long id, @NonNull long id1, @NonNull long id2, @NonNull String value);
}
