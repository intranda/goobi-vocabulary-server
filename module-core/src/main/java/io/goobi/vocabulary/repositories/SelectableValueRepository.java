package io.goobi.vocabulary.repositories;

import io.goobi.vocabulary.model.jpa.SelectableValueEntity;
import org.springframework.data.repository.CrudRepository;

public interface SelectableValueRepository extends CrudRepository<SelectableValueEntity, Long> {
}