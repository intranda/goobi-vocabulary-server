package io.goobi.vocabularyserver.service.manager;

import io.goobi.vocabularyserver.exception.EntityNotFoundException;
import io.goobi.vocabularyserver.exception.ValidationException;
import io.goobi.vocabularyserver.exchange.FieldTypeDTO;
import io.goobi.vocabularyserver.model.FieldType;
import io.goobi.vocabularyserver.model.Language;
import io.goobi.vocabularyserver.repositories.FieldTypeRepository;
import io.goobi.vocabularyserver.service.exchange.DTOMapper;
import io.goobi.vocabularyserver.validation.Validator;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;

@Service
public class FieldTypeManager implements Manager<FieldTypeDTO> {
    private final FieldTypeRepository fieldTypeRepository;
    private final DTOMapper modelMapper;
    private final Validator<FieldType> validator;

    public FieldTypeManager(FieldTypeRepository fieldTypeRepository, DTOMapper modelMapper, Validator<FieldType> validator) {
        this.fieldTypeRepository = fieldTypeRepository;
        this.modelMapper = modelMapper;
        this.validator = validator;
    }

    @Override
    public Page<FieldTypeDTO> listAll(Pageable pageable) {
        return fieldTypeRepository.findAll(pageable)
                .map(modelMapper::toDTO);
    }

    @Override
    public FieldTypeDTO get(long id) {
        return modelMapper.toDTO(
                fieldTypeRepository.findById(id)
                        .orElseThrow(() -> new EntityNotFoundException(FieldType.class, id))
        );
    }

    @Override
    public FieldTypeDTO create(FieldTypeDTO newFieldTypeDTO) throws ValidationException {
        FieldType jpaType = modelMapper.toEntity(newFieldTypeDTO);
        validator.validate(jpaType);
        return modelMapper.toDTO(fieldTypeRepository.save(jpaType));
    }

    @Override
    public FieldTypeDTO delete(long id) {
        if (!fieldTypeRepository.existsById(id)) {
            throw new EntityNotFoundException(Language.class, id);
        }
        fieldTypeRepository.deleteById(id);
        return null;
    }
}
