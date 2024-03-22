package io.goobi.vocabularyserver.service.manager;

import io.goobi.vocabularyserver.exception.EntityNotFoundException;
import io.goobi.vocabularyserver.exception.MissingValuesException;
import io.goobi.vocabularyserver.exception.UnsupportedEntityReplacementException;
import io.goobi.vocabularyserver.exchange.VocabularyDTO;
import io.goobi.vocabularyserver.model.Vocabulary;
import io.goobi.vocabularyserver.model.VocabularySchema;
import io.goobi.vocabularyserver.repositories.VocabularyRepository;
import io.goobi.vocabularyserver.repositories.VocabularySchemaRepository;
import io.goobi.vocabularyserver.service.exchange.ExchangeTypeTransformer;
import org.modelmapper.ModelMapper;
import org.springframework.stereotype.Service;

import java.util.LinkedList;
import java.util.List;
import java.util.stream.Collectors;

@Service
public class VocabularyManager implements Manager<VocabularyDTO> {
    private final VocabularyRepository vocabularyRepository;
    private final VocabularySchemaRepository vocabularySchemaRepository;
    private final ExchangeTypeTransformer exchangeTypeTransformer;

    private final ModelMapper modelMapper;

    public VocabularyManager(VocabularyRepository vocabularyRepository, VocabularySchemaRepository vocabularySchemaRepository, ExchangeTypeTransformer exchangeTypeTransformer, ModelMapper modelMapper) {
        this.vocabularyRepository = vocabularyRepository;
        this.vocabularySchemaRepository = vocabularySchemaRepository;
        this.exchangeTypeTransformer = exchangeTypeTransformer;
        this.modelMapper = modelMapper;
    }

    @Override
    public List<VocabularyDTO> listAll() {
        return vocabularyRepository.findAll()
                .stream()
                .map(exchangeTypeTransformer::transform)
                .collect(Collectors.toList());
    }

    @Override
    public VocabularyDTO get(long id) {
        return exchangeTypeTransformer.transform(
                vocabularyRepository.findById(id)
                        .orElseThrow(() -> new EntityNotFoundException(Vocabulary.class, id))
        );
    }

    @Override
    public VocabularyDTO create(VocabularyDTO newVocabularyDTO) {
        Vocabulary jpaVocabulary = modelMapper.map(newVocabularyDTO, Vocabulary.class);
        return modelMapper.map(vocabularyRepository.save(jpaVocabulary), VocabularyDTO.class);
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
        return exchangeTypeTransformer.transform(vocabularyRepository.save(jpaVocabulary));
    }

    private Vocabulary transformVocabulary(VocabularyDTO newVocabulary) {
        VocabularySchema jpaVocabularySchema = vocabularySchemaRepository
                .findById(newVocabulary.getSchemaId())
                .orElseThrow(() -> new EntityNotFoundException(VocabularySchema.class, newVocabulary.getSchemaId()));
        Vocabulary result = new Vocabulary(jpaVocabularySchema, newVocabulary.getName());
        result.setDescription(newVocabulary.getDescription());
        return result;
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
