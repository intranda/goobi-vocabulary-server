package io.goobi.vocabulary.repositories;

import io.goobi.vocabulary.model.FieldTypeEntity;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.repository.CrudRepository;

public interface FieldTypeRepository extends CrudRepository<FieldTypeEntity, Long> {
    Page<FieldTypeEntity> findAll(Pageable pageable);
}