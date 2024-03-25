package io.goobi.vocabularyserver.service.manager;

import io.goobi.vocabularyserver.exception.EntityNotFoundException;
import io.goobi.vocabularyserver.exception.MissingValuesException;
import io.goobi.vocabularyserver.exception.UnsupportedEntityReplacementException;
import io.goobi.vocabularyserver.exchange.VocabularyDTO;
import io.goobi.vocabularyserver.model.Vocabulary;
import io.goobi.vocabularyserver.repositories.VocabularyRepository;
import io.goobi.vocabularyserver.service.exchange.DTOMapper;
import org.springframework.stereotype.Service;

import java.util.LinkedList;
import java.util.List;
import java.util.stream.Collectors;

@Service
public class VocabularyManager implements Manager<VocabularyDTO> {
    private final VocabularyRepository vocabularyRepository;
    private final DTOMapper modelMapper;

    public VocabularyManager(VocabularyRepository vocabularyRepository, DTOMapper modelMapper) {
        this.vocabularyRepository = vocabularyRepository;
        this.modelMapper = modelMapper;
    }

    @Override
    public List<VocabularyDTO> listAll() {
        return vocabularyRepository.findAll()
                .stream()
                .map(modelMapper::toDTO)
                .collect(Collectors.toList());
    }

    @Override
    public VocabularyDTO get(long id) {
        return modelMapper.toDTO(
                vocabularyRepository.findById(id)
                    .orElseThrow(() -> new EntityNotFoundException(Vocabulary.class, id))
        );
    }

    @Override
    public VocabularyDTO create(VocabularyDTO newVocabularyDTO) {
        Vocabulary jpaVocabulary = modelMapper.toEntity(newVocabularyDTO);
        return modelMapper.toDTO(vocabularyRepository.save(jpaVocabulary));
    }

    // TODO: This is not working correctly: Impossible to pass id for replace insertion
    @Override
    public VocabularyDTO replace(VocabularyDTO newVocabularyDTO, long id) {
        Vocabulary jpaVocabulary = vocabularyRepository
                .findById(id)
                .orElseThrow(() -> new UnsupportedEntityReplacementException(newVocabularyDTO.getClass(), id));
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
    public VocabularyDTO delete(long id) {
        if (!vocabularyRepository.existsById(id)) {
            throw new EntityNotFoundException(Vocabulary.class, id);
        }
        vocabularyRepository.deleteById(id);
        return null;
    }
}
