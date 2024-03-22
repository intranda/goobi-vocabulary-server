package io.goobi.vocabularyserver.service.manager;

import io.goobi.vocabularyserver.exception.EntityNotFoundException;
import io.goobi.vocabularyserver.exchange.VocabularySchemaDTO;
import io.goobi.vocabularyserver.model.VocabularySchema;
import io.goobi.vocabularyserver.repositories.VocabularySchemaRepository;
import org.modelmapper.ModelMapper;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.stream.Collectors;

@Service
public class VocabularySchemaManager implements Manager<VocabularySchemaDTO> {

    private final VocabularySchemaRepository vocabularySchemaRepository;
    private final ModelMapper modelMapper;

    public VocabularySchemaManager(VocabularySchemaRepository vocabularySchemaRepository, ModelMapper modelMapper) {
        this.vocabularySchemaRepository = vocabularySchemaRepository;
        this.modelMapper = modelMapper;
    }

    @Override
    public List<VocabularySchemaDTO> listAll() {
        return vocabularySchemaRepository.findAll()
                .stream()
                .map(s -> modelMapper.map(s, VocabularySchemaDTO.class))
                .collect(Collectors.toList());
    }

    @Override
    public VocabularySchemaDTO get(long id) {
        return modelMapper.map(
                vocabularySchemaRepository.findById(id)
                        .orElseThrow(() -> new EntityNotFoundException(VocabularySchema.class, id)),
                VocabularySchemaDTO.class
        );
    }

    @Override
    public VocabularySchemaDTO create(VocabularySchemaDTO newSchema) {
        VocabularySchema jpaSchema = modelMapper.map(newSchema, VocabularySchema.class);
        return modelMapper.map(vocabularySchemaRepository.save(jpaSchema), VocabularySchemaDTO.class);
    }
}
