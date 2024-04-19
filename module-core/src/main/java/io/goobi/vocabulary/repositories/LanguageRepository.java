package io.goobi.vocabulary.repositories;

import io.goobi.vocabulary.model.LanguageEntity;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.repository.CrudRepository;
import org.springframework.lang.NonNull;

import java.util.Optional;

public interface LanguageRepository extends CrudRepository<LanguageEntity, Long> {
    Optional<LanguageEntity> findByAbbreviation(@NonNull String abbreviation);
    Page<LanguageEntity> findAll(Pageable pageable);
}