package io.goobi.vocabularyserver.repositories;

import io.goobi.vocabularyserver.model.FieldDefinitionEntity;
import org.springframework.data.repository.CrudRepository;

public interface FieldDefinitionRepository extends CrudRepository<FieldDefinitionEntity, Long> {
}