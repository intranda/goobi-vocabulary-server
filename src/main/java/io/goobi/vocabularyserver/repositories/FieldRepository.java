package io.goobi.vocabularyserver.repositories;

import io.goobi.vocabularyserver.model.Field;
import org.springframework.data.repository.CrudRepository;

import java.util.Optional;

public interface FieldRepository extends CrudRepository<Field, Integer> {
}
