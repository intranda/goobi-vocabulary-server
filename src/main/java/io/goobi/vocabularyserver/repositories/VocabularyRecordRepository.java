package io.goobi.vocabularyserver.repositories;

import io.goobi.vocabularyserver.model.VocabularyRecordEntity;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.repository.CrudRepository;
import org.springframework.lang.NonNull;

import java.util.Optional;

public interface VocabularyRecordRepository extends CrudRepository<VocabularyRecordEntity, Long> {
    @Override
    Optional<VocabularyRecordEntity> findById(Long aLong);

    Page<VocabularyRecordEntity> findByVocabulary_IdAndFields_FieldValues_Translations_ValueLike(@NonNull Long id, @NonNull String value, Pageable pageable);

    Page<VocabularyRecordEntity> findByVocabulary_IdAndParentRecordNull(@NonNull Long id, Pageable pageable);
}