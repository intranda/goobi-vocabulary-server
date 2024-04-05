package io.goobi.vocabularyserver.service.manager;

import io.goobi.vocabularyserver.exception.EntityNotFoundException;
import io.goobi.vocabularyserver.exception.MissingValuesException;
import io.goobi.vocabularyserver.exception.UnsupportedEntityReplacementException;
import io.goobi.vocabularyserver.exception.ValidationException;
import io.goobi.vocabularyserver.exchange.VocabularySchemaDTO;
import io.goobi.vocabularyserver.model.VocabularySchema;
import io.goobi.vocabularyserver.repositories.VocabularySchemaRepository;
import io.goobi.vocabularyserver.service.exchange.DTOMapper;
import io.goobi.vocabularyserver.validation.Validator;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;

import java.util.LinkedList;
import java.util.List;

@Service
public class VocabularySchemaManager implements Manager<VocabularySchemaDTO> {

    private final VocabularySchemaRepository vocabularySchemaRepository;
    private final DTOMapper modelMapper;
    private final Validator<VocabularySchema> validator;

    public VocabularySchemaManager(VocabularySchemaRepository vocabularySchemaRepository, DTOMapper modelMapper, Validator<VocabularySchema> validator) {
        this.vocabularySchemaRepository = vocabularySchemaRepository;
        this.modelMapper = modelMapper;
        this.validator = validator;
    }

    @Override
    public Page<VocabularySchemaDTO> listAll(Pageable pageable) {
        return vocabularySchemaRepository.findAll(pageable)
                .map(modelMapper::toDTO);
    }

    @Override
    public VocabularySchemaDTO get(long id) {
        return modelMapper.toDTO(
                vocabularySchemaRepository.findById(id)
                        .orElseThrow(() -> new EntityNotFoundException(VocabularySchema.class, id))
        );
    }

    @Override
    public VocabularySchemaDTO create(VocabularySchemaDTO newSchema) throws ValidationException {
        VocabularySchema jpaSchema = modelMapper.toEntity(newSchema);
        validator.validate(jpaSchema);
        return modelMapper.toDTO(vocabularySchemaRepository.save(jpaSchema));
    }

    // TODO: This is not working correctly: Impossible to pass id for replace insertion
    @Override
    public VocabularySchemaDTO replace(VocabularySchemaDTO newSchema) {
        VocabularySchema jpaSchema = vocabularySchemaRepository
                .findById(newSchema.getId())
                .orElseThrow(() -> new UnsupportedEntityReplacementException(newSchema.getClass(), newSchema.getId()));

        List<Runnable> replacements = new LinkedList<>();
        if (newSchema.getHierarchicalRecords() != null) {
            replacements.add(() -> jpaSchema.setHierarchicalRecords(newSchema.getHierarchicalRecords()));
        }
        if (replacements.isEmpty()) {
            throw new MissingValuesException(newSchema.getClass(), List.of("hierarchicalRecords"));
        }
        replacements.forEach(Runnable::run);
        return modelMapper.toDTO(vocabularySchemaRepository.save(jpaSchema));
    }

    @Override
    public VocabularySchemaDTO delete(long id) {
        if (!vocabularySchemaRepository.existsById(id)) {
            throw new EntityNotFoundException(VocabularySchema.class, id);
        }
        vocabularySchemaRepository.deleteById(id);
        return null;
    }
}
