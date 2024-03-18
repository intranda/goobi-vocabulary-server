package io.goobi.vocabularyserver.repositories;

import io.goobi.vocabularyserver.model.SelectableValue;
import org.springframework.data.repository.CrudRepository;

public interface SelectableValueRepository extends CrudRepository<SelectableValue, Long> {
}