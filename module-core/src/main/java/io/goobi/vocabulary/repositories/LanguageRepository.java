package io.goobi.vocabulary.repositories;

import io.goobi.vocabulary.model.jpa.LanguageEntity;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.repository.ListCrudRepository;
import org.springframework.lang.NonNull;

import java.util.Optional;

public interface LanguageRepository extends ListCrudRepository<LanguageEntity, Long> {
    Optional<LanguageEntity> findByAbbreviation(@NonNull String abbreviation);

    Page<LanguageEntity> findAll(Pageable pageable);
}