package io.goobi.vocabularyserver.repositories;

import io.goobi.vocabularyserver.model.FieldType;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.repository.CrudRepository;

public interface FieldTypeRepository extends CrudRepository<FieldType, Long> {
    Page<FieldType> findAll(Pageable pageable);
}