package io.goobi.vocabularyserver.repositories;

import io.goobi.vocabularyserver.model.Schema;
import org.springframework.data.repository.CrudRepository;

public interface SchemaRepository extends CrudRepository<Schema, Integer> {
}