package io.goobi.vocabularyserver.repositories;

import io.goobi.vocabularyserver.model.Language;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.repository.CrudRepository;
import org.springframework.lang.NonNull;

import java.util.Optional;

public interface LanguageRepository extends CrudRepository<Language, Long> {
    Optional<Language> findByAbbreviation(@NonNull String abbreviation);
    Page<Language> findAll(Pageable pageable);
}