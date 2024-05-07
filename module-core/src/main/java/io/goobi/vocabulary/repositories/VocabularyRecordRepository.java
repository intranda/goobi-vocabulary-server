package io.goobi.vocabulary.repositories;

import io.goobi.vocabulary.model.jpa.VocabularyRecordEntity;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.CrudRepository;
import org.springframework.lang.NonNull;

import java.util.Optional;

public interface VocabularyRecordRepository extends CrudRepository<VocabularyRecordEntity, Long> {
    @Override
    Optional<VocabularyRecordEntity> findById(Long aLong);

    Page<VocabularyRecordEntity> findByVocabulary_IdAndParentRecordNull(@NonNull Long id, Pageable pageable);

    @Query(value = "SELECT vre1_0.id,vre1_0.parent_record_id,vre1_0.vocabulary_id FROM vocabulary_record vre1_0 " +
            "join vocabulary v1_0 on v1_0.id=vre1_0.vocabulary_id " +
            "left join field_instance f1_0 on vre1_0.id=f1_0.record_id " +
            "left join field_value fv1_0 on f1_0.id=fv1_0.field_instance_id " +
            "left join field_translation t1_0 on fv1_0.id=t1_0.field_value_id " +
            "where v1_0.id=?1 and f1_0.field_definition_id=?2 and t1_0.content like ?3 escape '\\\\'", nativeQuery = true)
    Page<VocabularyRecordEntity> findRecordsInVocabularyByField(@NonNull long vocabularyId, @NonNull long fieldId, @NonNull String value, Pageable pageable);
}