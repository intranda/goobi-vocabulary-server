package io.goobi.vocabulary.repositories;

import io.goobi.vocabulary.model.FieldDefinitionEntity;
import org.springframework.data.repository.CrudRepository;

public interface FieldDefinitionRepository extends CrudRepository<FieldDefinitionEntity, Long> {
}