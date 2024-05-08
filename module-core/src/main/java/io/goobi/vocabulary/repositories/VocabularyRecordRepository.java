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

    /*
     * The outer SELECT queries all fields of the record (* doesn't work with JPA) joined on the inner SELECT.
     * The inner SELECT is responsible for generating a sorted list of record id's, sorted by the translated content only
     * for fields with the given field definition.
     */
    @Query(value = "SELECT r1.id,r1.parent_record_id,r1.vocabulary_id FROM (" +
            "SELECT DISTINCT r.id FROM vocabulary_record r " +
            "LEFT JOIN field_instance f ON r.id=f.record_id " +
            "LEFT JOIN field_value v ON f.id=v.field_instance_id " +
            "LEFT JOIN field_translation t ON v.id=t.field_value_id " +
            "WHERE r.vocabulary_id=?1 AND f.field_definition_id=?2 and r.parent_record_id IS NULL " +
            "ORDER BY t.content ASC" +
            ") AS sorted_records " +
            "LEFT JOIN vocabulary_record r1 ON sorted_records.id=r1.id", nativeQuery = true)
    Page<VocabularyRecordEntity> findByVocabulary_sortedByFieldDefinitionASC(@NonNull Long vocabularyId, @NonNull Long definitionId, Pageable pageable);
    @Query(value = "SELECT r1.id,r1.parent_record_id,r1.vocabulary_id FROM (" +
            "SELECT DISTINCT r.id FROM vocabulary_record r " +
            "LEFT JOIN field_instance f ON r.id=f.record_id " +
            "LEFT JOIN field_value v ON f.id=v.field_instance_id " +
            "LEFT JOIN field_translation t ON v.id=t.field_value_id " +
            "WHERE r.vocabulary_id=?1 AND f.field_definition_id=?2 and r.parent_record_id IS NULL " +
            "ORDER BY t.content DESC" +
            ") AS sorted_records " +
            "LEFT JOIN vocabulary_record r1 ON sorted_records.id=r1.id", nativeQuery = true)
    Page<VocabularyRecordEntity> findByVocabulary_sortedByFieldDefinitionDESC(@NonNull Long vocabularyId, @NonNull Long definitionId, Pageable pageable);

    Page<VocabularyRecordEntity> findByVocabulary_IdAndParentRecordNull(@NonNull Long vocabularyId, Pageable pageable);

    @Query(value = "SELECT vre1_0.id,vre1_0.parent_record_id,vre1_0.vocabulary_id FROM vocabulary_record vre1_0 " +
            "left join field_instance f1_0 on vre1_0.id=f1_0.record_id " +
            "left join field_value fv1_0 on f1_0.id=fv1_0.field_instance_id " +
            "left join field_translation t1_0 on fv1_0.id=t1_0.field_value_id " +
            "where vre1_0.vocabulary_id=?1 and t1_0.content like ?2 escape '\\\\'", nativeQuery = true)
    Page<VocabularyRecordEntity> findRecordsInVocabulary(@NonNull long vocabularyId, @NonNull String value, Pageable pageable);

    @Query(value = "SELECT vre1_0.id,vre1_0.parent_record_id,vre1_0.vocabulary_id FROM vocabulary_record vre1_0 " +
            "left join field_instance f1_0 on vre1_0.id=f1_0.record_id " +
            "left join field_value fv1_0 on f1_0.id=fv1_0.field_instance_id " +
            "left join field_translation t1_0 on fv1_0.id=t1_0.field_value_id " +
            "where vre1_0.vocabulary_id=?1 and f1_0.field_definition_id=?2 and t1_0.content like ?3 escape '\\\\'", nativeQuery = true)
    Page<VocabularyRecordEntity> findRecordsInVocabularyByField(@NonNull long vocabularyId, @NonNull long fieldId, @NonNull String value, Pageable pageable);
}