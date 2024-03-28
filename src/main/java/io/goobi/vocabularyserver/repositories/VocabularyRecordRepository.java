package io.goobi.vocabularyserver.repositories;

import io.goobi.vocabularyserver.model.VocabularyRecord;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.repository.CrudRepository;
import org.springframework.lang.NonNull;

import java.util.Optional;

public interface VocabularyRecordRepository extends CrudRepository<VocabularyRecord, Long> {
    @Override
    Optional<VocabularyRecord> findById(Long aLong);

//    Page<VocabularyRecord> findByVocabulary_IdAndFields_ValueLikeIgnoreCase(@NonNull Long id, @NonNull String value, Pageable pageables);

    Page<VocabularyRecord> findByVocabulary_Id(@NonNull Long id, Pageable pageable);

    Page<VocabularyRecord> findByVocabulary_IdAndFields_FieldValues_Translations_ValueLikeIgnoreCase(@NonNull Long id, @NonNull String value, Pageable pageable);
}