package io.goobi.vocabularyserver.service.manager;

import io.goobi.vocabularyserver.exception.EntityNotFoundException;
import io.goobi.vocabularyserver.exception.MissingValuesException;
import io.goobi.vocabularyserver.exception.UnsupportedEntityReplacementException;
import io.goobi.vocabularyserver.exception.ValidationException;
import io.goobi.vocabularyserver.exchange.FieldType;
import io.goobi.vocabularyserver.model.FieldTypeEntity;
import io.goobi.vocabularyserver.model.LanguageEntity;
import io.goobi.vocabularyserver.model.SelectableValueEntity;
import io.goobi.vocabularyserver.repositories.FieldTypeRepository;
import io.goobi.vocabularyserver.service.exchange.DTOMapper;
import io.goobi.vocabularyserver.validation.Validator;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;

import java.util.LinkedList;
import java.util.List;

@Service
public class FieldTypeManager implements Manager<FieldType> {
    private final FieldTypeRepository fieldTypeRepository;
    private final DTOMapper modelMapper;
    private final Validator<FieldTypeEntity> validator;

    public FieldTypeManager(FieldTypeRepository fieldTypeRepository, DTOMapper modelMapper, Validator<FieldTypeEntity> validator) {
        this.fieldTypeRepository = fieldTypeRepository;
        this.modelMapper = modelMapper;
        this.validator = validator;
    }

    @Override
    public Page<FieldType> listAll(Pageable pageable) {
        return fieldTypeRepository.findAll(pageable)
                .map(modelMapper::toDTO);
    }

    @Override
    public FieldType get(long id) {
        return modelMapper.toDTO(
                fieldTypeRepository.findById(id)
                        .orElseThrow(() -> new EntityNotFoundException(FieldTypeEntity.class, id))
        );
    }

    @Override
    public FieldType create(FieldType newFieldTypeDTO) throws ValidationException {
        FieldTypeEntity jpaType = modelMapper.toEntity(newFieldTypeDTO);
        validator.validate(jpaType);
        return modelMapper.toDTO(fieldTypeRepository.save(jpaType));
    }

    @Override
    public FieldType replace(FieldType newFieldTypeDTO) throws ValidationException {
        FieldTypeEntity jpaFieldType = fieldTypeRepository
                .findById(newFieldTypeDTO.getId())
                .orElseThrow(() -> new UnsupportedEntityReplacementException(newFieldTypeDTO.getClass(), newFieldTypeDTO.getId()));

        FieldTypeEntity transformed = modelMapper.toEntity(newFieldTypeDTO);

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
                List<SelectableValueEntity> selectableValues = transformed.getSelectableValues();
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
    public FieldType delete(long id) {
        if (!fieldTypeRepository.existsById(id)) {
            throw new EntityNotFoundException(LanguageEntity.class, id);
        }
        fieldTypeRepository.deleteById(id);
        return null;
    }
}
