package io.goobi.vocabularyserver.service.manager;

import io.goobi.vocabularyserver.exception.EntityNotFoundException;
import io.goobi.vocabularyserver.exception.MissingValuesException;
import io.goobi.vocabularyserver.exception.UnsupportedEntityReplacementException;
import io.goobi.vocabularyserver.exception.ValidationException;
import io.goobi.vocabularyserver.exchange.FieldTypeDTO;
import io.goobi.vocabularyserver.model.FieldType;
import io.goobi.vocabularyserver.model.Language;
import io.goobi.vocabularyserver.model.SelectableValue;
import io.goobi.vocabularyserver.repositories.FieldTypeRepository;
import io.goobi.vocabularyserver.service.exchange.DTOMapper;
import io.goobi.vocabularyserver.validation.Validator;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;

import java.util.LinkedList;
import java.util.List;

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
    public FieldTypeDTO replace(FieldTypeDTO newFieldTypeDTO) throws ValidationException {
        FieldType jpaFieldType = fieldTypeRepository
                .findById(newFieldTypeDTO.getId())
                .orElseThrow(() -> new UnsupportedEntityReplacementException(newFieldTypeDTO.getClass(), newFieldTypeDTO.getId()));

        FieldType transformed = modelMapper.toEntity(newFieldTypeDTO);

        List<Runnable> replacements = new LinkedList<>();
        if (newFieldTypeDTO.getName() != null) {
            replacements.add(() -> jpaFieldType.setName(newFieldTypeDTO.getName()));
        }
        if (newFieldTypeDTO.getValidation() != null) {
            if (newFieldTypeDTO.getValidation().isBlank()) {
                replacements.add(() -> jpaFieldType.setValidation(null));
            } else {
                replacements.add(() -> jpaFieldType.setValidation(newFieldTypeDTO.getValidation()));
            }
        }
        if (newFieldTypeDTO.getSelectableValues() != null) {
            replacements.add(() -> {
                jpaFieldType.getSelectableValues().clear();
                List<SelectableValue> selectableValues = transformed.getSelectableValues();
                selectableValues.forEach(sv -> sv.setFieldType(jpaFieldType));
                jpaFieldType.getSelectableValues().addAll(selectableValues);
            });
        }
        if (replacements.isEmpty()) {
            throw new MissingValuesException(newFieldTypeDTO.getClass(), List.of("name", "validation", "selectableValues"));
        }
        replacements.forEach(Runnable::run);
        validator.validate(jpaFieldType);
        return modelMapper.toDTO(fieldTypeRepository.save(jpaFieldType));
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
