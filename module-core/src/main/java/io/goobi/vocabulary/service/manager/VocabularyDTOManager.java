package io.goobi.vocabulary.service.manager;

import io.goobi.vocabulary.exception.DeletionOfReferencedVocabularyException;
import io.goobi.vocabulary.exception.EntityNotFoundException;
import io.goobi.vocabulary.exception.MissingValuesException;
import io.goobi.vocabulary.exception.UnsupportedEntityReplacementException;
import io.goobi.vocabulary.exchange.Vocabulary;
import io.goobi.vocabulary.model.jpa.VocabularyEntity;
import io.goobi.vocabulary.repositories.VocabularyRepository;
import io.goobi.vocabulary.service.exchange.DTOMapper;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;

import java.util.LinkedList;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

@Service
public class VocabularyDTOManager implements Manager<Vocabulary> {
    private final VocabularyRepository vocabularyRepository;
    private final DTOMapper modelMapper;

    public VocabularyDTOManager(VocabularyRepository vocabularyRepository, DTOMapper modelMapper) {
        this.vocabularyRepository = vocabularyRepository;
        this.modelMapper = modelMapper;
    }

    @Override
    public List<Vocabulary> listAll() {
        return vocabularyRepository.findAll().stream()
                .map(modelMapper::toDTO)
                .collect(Collectors.toList());
    }

    @Override
    public Page<Vocabulary> list(Pageable pageable) {
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

    public Vocabulary find(String name) {
        return modelMapper.toDTO(
                vocabularyRepository.findByName(name)
                        .orElseThrow(() -> new EntityNotFoundException(VocabularyEntity.class, name))
        );
    }

    @Override
    public Vocabulary create(Vocabulary newVocabularyDTO) {
        VocabularyEntity jpaVocabulary = modelMapper.toEntity(newVocabularyDTO);
        // TODO: Vocabulary validation - Metadata schema must be single-root and non-hierarchical
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
        VocabularyEntity vocabulary = vocabularyRepository.findById(id)
                .orElseThrow(() -> new EntityNotFoundException(VocabularyEntity.class, id));
        checkForExistingReferencesToVocabulary(vocabulary);
        vocabularyRepository.deleteById(id);
        return null;
    }

    private void checkForExistingReferencesToVocabulary(VocabularyEntity vocabulary) {
        Set<VocabularyEntity> referencingVocabularies = vocabularyRepository.findDistinctBySchema_Definitions_ReferenceVocabulary(vocabulary);
        if (!referencingVocabularies.isEmpty()) {
            throw new DeletionOfReferencedVocabularyException(vocabulary, referencingVocabularies);
        }
    }
}
