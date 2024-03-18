package io.goobi.vocabularyserver.repositories;

import io.goobi.vocabularyserver.model.Record;
import org.springframework.data.repository.CrudRepository;

public interface RecordRepository extends CrudRepository<Record, Long> {
}