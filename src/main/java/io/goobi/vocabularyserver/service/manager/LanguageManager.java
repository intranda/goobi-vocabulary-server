package io.goobi.vocabularyserver.service.manager;

import io.goobi.vocabularyserver.exception.EntityNotFoundException;
import io.goobi.vocabularyserver.exception.MissingValuesException;
import io.goobi.vocabularyserver.exception.UnsupportedEntityReplacementException;
import io.goobi.vocabularyserver.exception.ValidationException;
import io.goobi.vocabularyserver.exchange.Language;
import io.goobi.vocabularyserver.model.LanguageEntity;
import io.goobi.vocabularyserver.repositories.LanguageRepository;
import io.goobi.vocabularyserver.service.exchange.DTOMapper;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;

import java.util.LinkedList;
import java.util.List;

@Service
public class LanguageManager implements Manager<Language> {
    private final LanguageRepository languageRepository;
    private final DTOMapper modelMapper;

    public LanguageManager(LanguageRepository languageRepository, DTOMapper modelMapper) {
        this.languageRepository = languageRepository;
        this.modelMapper = modelMapper;
    }

    @Override
    public Page<Language> listAll(Pageable pageable) {
        return languageRepository.findAll(pageable)
                .map(modelMapper::toDTO);
    }

    @Override
    public Language get(long id) {
        return modelMapper.toDTO(
                languageRepository.findById(id)
                        .orElseThrow(() -> new EntityNotFoundException(LanguageEntity.class, id))
        );
    }

    @Override
    public Language create(Language newLanguageDTO) throws ValidationException {
        LanguageEntity jpaLanguage = modelMapper.toEntity(newLanguageDTO);
        return modelMapper.toDTO(languageRepository.save(jpaLanguage));
    }

    @Override
    public Language replace(Language newLanguageDTO) {
        LanguageEntity jpaLanguage = languageRepository
                .findById(newLanguageDTO.getId())
                .orElseThrow(() -> new UnsupportedEntityReplacementException(newLanguageDTO.getClass(), newLanguageDTO.getId()));

        List<Runnable> replacements = new LinkedList<>();
        if (newLanguageDTO.getAbbreviation() != null) {
            replacements.add(() -> jpaLanguage.setAbbreviation(newLanguageDTO.getAbbreviation()));
        }
        if (newLanguageDTO.getName() != null) {
            replacements.add(() -> jpaLanguage.setName(newLanguageDTO.getName()));
        }
        if (replacements.isEmpty()) {
            throw new MissingValuesException(newLanguageDTO.getClass(), List.of("abbreviation", "name"));
        }
        replacements.forEach(Runnable::run);
        return modelMapper.toDTO(languageRepository.save(jpaLanguage));
    }

    @Override
    public Language delete(long id) {
        if (!languageRepository.existsById(id)) {
            throw new EntityNotFoundException(LanguageEntity.class, id);
        }
        languageRepository.deleteById(id);
        return null;
    }
}
