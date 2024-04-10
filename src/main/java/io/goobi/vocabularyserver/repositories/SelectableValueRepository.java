package io.goobi.vocabularyserver.repositories;

import io.goobi.vocabularyserver.model.SelectableValueEntity;
import org.springframework.data.repository.CrudRepository;

public interface SelectableValueRepository extends CrudRepository<SelectableValueEntity, Long> {
}