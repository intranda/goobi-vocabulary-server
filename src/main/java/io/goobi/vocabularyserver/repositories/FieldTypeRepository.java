package io.goobi.vocabularyserver.repositories;

import io.goobi.vocabularyserver.model.FieldTypeEntity;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.repository.CrudRepository;

public interface FieldTypeRepository extends CrudRepository<FieldTypeEntity, Long> {
    Page<FieldTypeEntity> findAll(Pageable pageable);
}