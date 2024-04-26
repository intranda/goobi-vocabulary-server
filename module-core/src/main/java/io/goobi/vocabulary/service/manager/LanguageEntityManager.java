package io.goobi.vocabulary.service.manager;

import io.goobi.vocabulary.exception.EntityNotFoundException;
import io.goobi.vocabulary.exception.MissingValuesException;
import io.goobi.vocabulary.exception.UnsupportedEntityReplacementException;
import io.goobi.vocabulary.exception.ValidationException;
import io.goobi.vocabulary.model.jpa.LanguageEntity;
import io.goobi.vocabulary.repositories.LanguageRepository;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;

import java.util.LinkedList;
import java.util.List;

@Service
public class LanguageEntityManager implements Manager<LanguageEntity> {
    private final LanguageRepository languageRepository;

    public LanguageEntityManager(LanguageRepository languageRepository) {
        this.languageRepository = languageRepository;
    }

    @Override
    public Page<LanguageEntity> listAll(Pageable pageable) {
        return languageRepository.findAll(pageable);
    }

    @Override
    public LanguageEntity get(long id) {
        return languageRepository.findById(id)
                        .orElseThrow(() -> new EntityNotFoundException(LanguageEntity.class, id));
    }

    @Override
    public LanguageEntity create(LanguageEntity newLanguage) throws ValidationException {
        return languageRepository.save(newLanguage);
    }

    @Override
    public LanguageEntity replace(LanguageEntity newLanguage) {
        LanguageEntity jpaLanguage = languageRepository
                .findById(newLanguage.getId())
                .orElseThrow(() -> new UnsupportedEntityReplacementException(newLanguage.getClass(), newLanguage.getId()));

        List<Runnable> replacements = new LinkedList<>();
        if (newLanguage.getAbbreviation() != null) {
            replacements.add(() -> jpaLanguage.setAbbreviation(newLanguage.getAbbreviation()));
        }
        if (newLanguage.getName() != null) {
            replacements.add(() -> jpaLanguage.setName(newLanguage.getName()));
        }
        if (replacements.isEmpty()) {
            throw new MissingValuesException(newLanguage.getClass(), List.of("abbreviation", "name"));
        }
        replacements.forEach(Runnable::run);
        return languageRepository.save(jpaLanguage);
    }

    @Override
    public LanguageEntity delete(long id) {
        if (!languageRepository.existsById(id)) {
            throw new EntityNotFoundException(LanguageEntity.class, id);
        }
        languageRepository.deleteById(id);
        return null;
    }
}
