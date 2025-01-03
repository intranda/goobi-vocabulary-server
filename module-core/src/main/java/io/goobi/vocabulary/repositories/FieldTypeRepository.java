package io.goobi.vocabulary.repositories;

import io.goobi.vocabulary.model.jpa.FieldTypeEntity;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.repository.ListCrudRepository;
import org.springframework.lang.NonNull;

import java.util.Optional;

public interface FieldTypeRepository extends ListCrudRepository<FieldTypeEntity, Long> {
    Page<FieldTypeEntity> findAll(Pageable pageable);

    Optional<FieldTypeEntity> findByName(@NonNull String name);
}