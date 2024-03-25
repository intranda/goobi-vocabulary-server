package io.goobi.vocabularyserver.service.manager;

import io.goobi.vocabularyserver.exception.EntityNotFoundException;
import io.goobi.vocabularyserver.exchange.VocabularySchemaDTO;
import io.goobi.vocabularyserver.model.VocabularySchema;
import io.goobi.vocabularyserver.repositories.VocabularySchemaRepository;
import io.goobi.vocabularyserver.service.exchange.DTOMapper;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.stream.Collectors;

@Service
public class VocabularySchemaManager implements Manager<VocabularySchemaDTO> {

    private final VocabularySchemaRepository vocabularySchemaRepository;
    private final DTOMapper modelMapper;

    public VocabularySchemaManager(VocabularySchemaRepository vocabularySchemaRepository, DTOMapper modelMapper) {
        this.vocabularySchemaRepository = vocabularySchemaRepository;
        this.modelMapper = modelMapper;
    }

    @Override
    public List<VocabularySchemaDTO> listAll() {
        return vocabularySchemaRepository.findAll()
                .stream()
                .map(modelMapper::toDTO)
                .collect(Collectors.toList());
    }

    @Override
    public VocabularySchemaDTO get(long id) {
        return modelMapper.toDTO(
                vocabularySchemaRepository.findById(id)
                        .orElseThrow(() -> new EntityNotFoundException(VocabularySchema.class, id))
        );
    }

    @Override
    public VocabularySchemaDTO create(VocabularySchemaDTO newSchema) {
        VocabularySchema jpaSchema = modelMapper.toEntity(newSchema);
        return modelMapper.toDTO(vocabularySchemaRepository.save(jpaSchema));
    }
}
