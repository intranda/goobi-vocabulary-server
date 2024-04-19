package io.goobi.vocabulary.service.manager;

import io.goobi.vocabulary.exception.EntityNotFoundException;
import io.goobi.vocabulary.exception.MissingValuesException;
import io.goobi.vocabulary.exception.UnsupportedEntityReplacementException;
import io.goobi.vocabulary.exchange.Vocabulary;
import io.goobi.vocabulary.model.VocabularyEntity;
import io.goobi.vocabulary.repositories.VocabularyRepository;
import io.goobi.vocabulary.service.exchange.DTOMapper;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;

import java.util.LinkedList;
import java.util.List;

@Service
public class VocabularyDTOManager implements Manager<Vocabulary> {
    private final VocabularyRepository vocabularyRepository;
    private final DTOMapper modelMapper;

    public VocabularyDTOManager(VocabularyRepository vocabularyRepository, DTOMapper modelMapper) {
        this.vocabularyRepository = vocabularyRepository;
        this.modelMapper = modelMapper;
    }

    @Override
    public Page<Vocabulary> listAll(Pageable pageable) {
        return vocabularyRepository.findAll(pageable)
                .map(modelMapper::toDTO);
    }

    @Override
    public Vocabulary get(long id) {
        return modelMapper.toDTO(
                vocabularyRepository.findById(id)
                    .orElseThrow(() -> new EntityNotFoundException(VocabularyEntity.class, id))
        );
    }

    @Override
    public Vocabulary create(Vocabulary newVocabularyDTO) {
        VocabularyEntity jpaVocabulary = modelMapper.toEntity(newVocabularyDTO);
        return modelMapper.toDTO(vocabularyRepository.save(jpaVocabulary));
    }

    // TODO: This is not working correctly: Impossible to pass id for replace insertion
    @Override
    public Vocabulary replace(Vocabulary newVocabularyDTO) {
        VocabularyEntity jpaVocabulary = vocabularyRepository
                .findById(newVocabularyDTO.getId())
                .orElseThrow(() -> new UnsupportedEntityReplacementException(newVocabularyDTO.getClass(), newVocabularyDTO.getId()));
//                .orElseGet(() -> transformVocabulary(newVocabulary));

        List<Runnable> replacements = new LinkedList<>();
        if (newVocabularyDTO.getName() != null) {
            replacements.add(() -> jpaVocabulary.setName(newVocabularyDTO.getName()));
        }
        if (newVocabularyDTO.getDescription() != null) {
            replacements.add(() -> jpaVocabulary.setDescription(newVocabularyDTO.getDescription()));
        }
        if (replacements.isEmpty()) {
            throw new MissingValuesException(newVocabularyDTO.getClass(), List.of("name", "description"));
        }
        replacements.forEach(Runnable::run);
        return modelMapper.toDTO(vocabularyRepository.save(jpaVocabulary));
    }

    @Override
    public Vocabulary delete(long id) {
        if (!vocabularyRepository.existsById(id)) {
            throw new EntityNotFoundException(VocabularyEntity.class, id);
        }
        vocabularyRepository.deleteById(id);
        return null;
    }
}
