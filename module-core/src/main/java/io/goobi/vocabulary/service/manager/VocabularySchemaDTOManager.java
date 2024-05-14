package io.goobi.vocabulary.service.manager;

import io.goobi.vocabulary.exception.EntityNotFoundException;
import io.goobi.vocabulary.exception.MissingValuesException;
import io.goobi.vocabulary.exception.UnsupportedEntityReplacementException;
import io.goobi.vocabulary.exception.ValidationException;
import io.goobi.vocabulary.exchange.VocabularySchema;
import io.goobi.vocabulary.model.jpa.VocabularySchemaEntity;
import io.goobi.vocabulary.repositories.VocabularySchemaRepository;
import io.goobi.vocabulary.service.exchange.DTOMapper;
import io.goobi.vocabulary.validation.Validator;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;

import java.util.LinkedList;
import java.util.List;

@Service
public class VocabularySchemaDTOManager implements Manager<VocabularySchema> {

    private final VocabularySchemaRepository vocabularySchemaRepository;
    private final DTOMapper modelMapper;
    private final Validator<VocabularySchemaEntity> validator;

    public VocabularySchemaDTOManager(VocabularySchemaRepository vocabularySchemaRepository, DTOMapper modelMapper, Validator<VocabularySchemaEntity> validator) {
        this.vocabularySchemaRepository = vocabularySchemaRepository;
        this.modelMapper = modelMapper;
        this.validator = validator;
    }

    @Override
    public Page<VocabularySchema> list(Pageable pageable) {
        return vocabularySchemaRepository.findAll(pageable)
                .map(modelMapper::toDTO);
    }

    @Override
    public VocabularySchema get(long id) {
        return modelMapper.toDTO(
                vocabularySchemaRepository.findById(id)
                        .orElseThrow(() -> new EntityNotFoundException(VocabularySchemaEntity.class, id))
        );
    }

    @Override
    public VocabularySchema create(VocabularySchema newSchema) throws ValidationException {
        VocabularySchemaEntity jpaSchema = modelMapper.toEntity(newSchema);
        validator.validate(jpaSchema);
        return modelMapper.toDTO(vocabularySchemaRepository.save(jpaSchema));
    }

    // TODO: This is not working correctly: Impossible to pass id for replace insertion
    @Override
    public VocabularySchema replace(VocabularySchema newSchema) {
        VocabularySchemaEntity jpaSchema = vocabularySchemaRepository
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
    public VocabularySchema delete(long id) {
        if (!vocabularySchemaRepository.existsById(id)) {
            throw new EntityNotFoundException(VocabularySchemaEntity.class, id);
        }
        vocabularySchemaRepository.deleteById(id);
        return null;
    }
}
