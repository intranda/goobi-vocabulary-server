package io.goobi.vocabulary.service.manager;

import io.goobi.vocabulary.exception.EntityNotFoundException;
import io.goobi.vocabulary.exchange.FieldDefinition;
import io.goobi.vocabulary.repositories.FieldDefinitionRepository;
import io.goobi.vocabulary.service.exchange.DTOMapper;
import org.springframework.stereotype.Service;

@Service
public class FieldDefinitionDTOManager {
    private final FieldDefinitionRepository fieldDefinitionRepository;
    private final DTOMapper modelMapper;

    public FieldDefinitionDTOManager(FieldDefinitionRepository fieldDefinitionRepository, DTOMapper modelMapper) {
        this.fieldDefinitionRepository = fieldDefinitionRepository;
        this.modelMapper = modelMapper;
    }

    public FieldDefinition get(long id) {
        return modelMapper.toDTO(
                fieldDefinitionRepository.findById(id)
                        .orElseThrow(() -> new EntityNotFoundException(FieldDefinition.class, id))
        );
    }
}
