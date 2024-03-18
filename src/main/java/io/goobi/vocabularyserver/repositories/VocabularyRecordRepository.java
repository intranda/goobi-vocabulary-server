package io.goobi.vocabularyserver.repositories;

import io.goobi.vocabularyserver.model.VocabularyRecord;
import org.springframework.data.repository.CrudRepository;
import org.springframework.lang.NonNull;

import java.util.Optional;

public interface VocabularyRecordRepository extends CrudRepository<VocabularyRecord, Long> {
    Optional<VocabularyRecord> findByFields_ValueLike(@NonNull String value);

    @Override
    Optional<VocabularyRecord> findById(Long aLong);
}