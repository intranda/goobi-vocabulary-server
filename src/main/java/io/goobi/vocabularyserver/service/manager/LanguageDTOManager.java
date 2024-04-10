package io.goobi.vocabularyserver.service.manager;

import io.goobi.vocabularyserver.exception.ValidationException;
import io.goobi.vocabularyserver.exchange.Language;
import io.goobi.vocabularyserver.model.LanguageEntity;
import io.goobi.vocabularyserver.service.exchange.DTOMapper;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;

@Service
public class LanguageDTOManager implements Manager<Language> {
    private final Manager<LanguageEntity> languageEntityManager;
    private final DTOMapper modelMapper;

    public LanguageDTOManager(Manager<LanguageEntity> languageEntityManager, DTOMapper modelMapper) {
        this.languageEntityManager = languageEntityManager;
        this.modelMapper = modelMapper;
    }

    @Override
    public Page<Language> listAll(Pageable pageable) {
        return languageEntityManager.listAll(pageable)
                .map(modelMapper::toDTO);
    }

    @Override
    public Language get(long id) {
        return modelMapper.toDTO(languageEntityManager.get(id));
    }

    @Override
    public Language create(Language newLanguageDTO) throws ValidationException {
        return modelMapper.toDTO(languageEntityManager.create(modelMapper.toEntity(newLanguageDTO)));
    }

    @Override
    public Language replace(Language newLanguageDTO) throws ValidationException {
        return modelMapper.toDTO(languageEntityManager.replace(modelMapper.toEntity(newLanguageDTO)));
    }

    @Override
    public Language delete(long id) {
        languageEntityManager.delete(id);
        return null;
    }
}
