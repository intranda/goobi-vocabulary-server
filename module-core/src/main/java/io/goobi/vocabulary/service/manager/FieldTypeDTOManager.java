package io.goobi.vocabulary.service.manager;

import io.goobi.vocabulary.exception.EntityNotFoundException;
import io.goobi.vocabulary.exception.MissingValuesException;
import io.goobi.vocabulary.exception.UnsupportedEntityReplacementException;
import io.goobi.vocabulary.exception.ValidationException;
import io.goobi.vocabulary.exchange.FieldType;
import io.goobi.vocabulary.model.jpa.FieldTypeEntity;
import io.goobi.vocabulary.model.jpa.LanguageEntity;
import io.goobi.vocabulary.model.jpa.SelectableValueEntity;
import io.goobi.vocabulary.repositories.FieldTypeRepository;
import io.goobi.vocabulary.service.exchange.DTOMapper;
import io.goobi.vocabulary.validation.Validator;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;

import java.util.LinkedList;
import java.util.List;

@Service
public class FieldTypeDTOManager implements Manager<FieldType> {
    private final FieldTypeRepository fieldTypeRepository;
    private final DTOMapper modelMapper;
    private final Validator<FieldTypeEntity> validator;

    public FieldTypeDTOManager(FieldTypeRepository fieldTypeRepository, DTOMapper modelMapper, Validator<FieldTypeEntity> validator) {
        this.fieldTypeRepository = fieldTypeRepository;
        this.modelMapper = modelMapper;
        this.validator = validator;
    }

    @Override
    public Page<FieldType> list(Pageable pageable) {
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

    public FieldType find(String name) {
        return modelMapper.toDTO(
                fieldTypeRepository.findByName(name)
                        .orElseThrow(() -> new EntityNotFoundException(FieldTypeEntity.class, name))
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
