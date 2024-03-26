package io.goobi.vocabularyserver.repositories;

import io.goobi.vocabularyserver.model.VocabularyRecord;
import org.springframework.data.repository.CrudRepository;
import org.springframework.lang.NonNull;

import java.util.List;
import java.util.Optional;

public interface VocabularyRecordRepository extends CrudRepository<VocabularyRecord, Long> {
    @Override
    Optional<VocabularyRecord> findById(Long aLong);

    List<VocabularyRecord> findByVocabulary_IdAndFields_ValueLikeIgnoreCase(@NonNull Long id, @NonNull String value);
}