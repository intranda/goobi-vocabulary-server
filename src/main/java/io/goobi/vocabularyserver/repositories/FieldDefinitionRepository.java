package io.goobi.vocabularyserver.repositories;

import io.goobi.vocabularyserver.model.FieldDefinition;
import org.springframework.data.repository.CrudRepository;

public interface FieldDefinitionRepository extends CrudRepository<FieldDefinition, Long> {
}