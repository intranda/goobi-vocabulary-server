package io.goobi.vocabulary.service.manager;

import io.goobi.vocabulary.exception.EntityNotFoundException;
import io.goobi.vocabulary.exception.MissingValuesException;
import io.goobi.vocabulary.exception.RecordValidationException;
import io.goobi.vocabulary.exception.UnsupportedEntityReplacementException;
import io.goobi.vocabulary.exception.ValidationException;
import io.goobi.vocabulary.exchange.VocabularyRecord;
import io.goobi.vocabulary.model.jpa.FieldInstanceEntity;
import io.goobi.vocabulary.model.jpa.VocabularyRecordEntity;
import io.goobi.vocabulary.repositories.VocabularyRecordRepository;
import io.goobi.vocabulary.service.exchange.DTOMapper;
import io.goobi.vocabulary.validation.Validator;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;

import java.util.Collections;
import java.util.LinkedList;
import java.util.List;
import java.util.stream.Collectors;

@Service
public class RecordDTOManager implements Manager<VocabularyRecord> {
    public static final String SEARCH_QUERY_DELIMITER = ":";
    private final VocabularyRecordRepository vocabularyRecordRepository;
    private final DTOMapper modelMapper;
    private final Validator<VocabularyRecordEntity> validator;

    public RecordDTOManager(VocabularyRecordRepository vocabularyRecordRepository, DTOMapper modelMapper, Validator<VocabularyRecordEntity> validator) {
        this.vocabularyRecordRepository = vocabularyRecordRepository;
        this.modelMapper = modelMapper;
        this.validator = validator;
    }

    public Page<VocabularyRecord> listAll(long id, Pageable pageRequest) {
        return vocabularyRecordRepository.findByVocabulary_IdAndParentRecordNull(id, pageRequest)
                .map(modelMapper::toDTO);
    }

    @Override
    public VocabularyRecord get(long id) {
        return modelMapper.toDTO(
                vocabularyRecordRepository.findById(id)
                        .orElseThrow(() -> new EntityNotFoundException(VocabularyRecordEntity.class, id))
        );
    }

    @Override
    public VocabularyRecord create(VocabularyRecord newRecord) throws ValidationException {
        VocabularyRecordEntity jpaVocabularyRecord = modelMapper.toEntity(newRecord);
        validator.validate(jpaVocabularyRecord);
        return modelMapper.toDTO(vocabularyRecordRepository.save(jpaVocabularyRecord));
    }

    public VocabularyRecord createSubRecord(VocabularyRecord newRecord) throws ValidationException {
        VocabularyRecordEntity jpaParent = vocabularyRecordRepository.findById(newRecord.getParentId())
                .orElseThrow(() -> new EntityNotFoundException(VocabularyRecordEntity.class, newRecord.getParentId()));
        newRecord.setVocabularyId(jpaParent.getVocabulary().getId());
        VocabularyRecordEntity jpaNewChildRecord = modelMapper.toEntity(newRecord);
        jpaNewChildRecord.setParentRecord(jpaParent);
        jpaParent.getChildren().add(jpaNewChildRecord);
        recursiveRecordValidation(jpaParent);
        jpaNewChildRecord = vocabularyRecordRepository.save(jpaNewChildRecord);
        vocabularyRecordRepository.save(jpaParent);
        return modelMapper.toDTO(jpaNewChildRecord);
    }

    private void recursiveRecordValidation(VocabularyRecordEntity recordToValidate) throws ValidationException {
        List<Throwable> errors = new LinkedList<>();
        try {
            validator.validate(recordToValidate);
        } catch (ValidationException e) {
            errors.add(e);
        }
        for (VocabularyRecordEntity r : recordToValidate.getChildren()) {
            try {
                validator.validate(r);
            } catch (ValidationException e) {
                errors.add(e);
            }
        }
        if (!errors.isEmpty()) {
            throw new RecordValidationException(errors.stream().map(Throwable::getMessage).collect(Collectors.joining("\n")));
        }
    }

    @Override
    public VocabularyRecord replace(VocabularyRecord newRecord) throws ValidationException {
        VocabularyRecordEntity jpaRecord = vocabularyRecordRepository
                .findById(newRecord.getId())
                .orElseThrow(() -> new UnsupportedEntityReplacementException(newRecord.getClass(), newRecord.getId()));

        List<Runnable> replacements = new LinkedList<>();
        if (!newRecord.getFields().isEmpty()) {
            replacements.add(() -> jpaRecord.getFields().clear());
            newRecord.getFields().stream()
                    .map(modelMapper::toEntity)
                    .forEach(f -> replacements.add(() -> jpaRecord.getFields().add(f)));
        }
        // TODO: Plan children replacement
        if (replacements.isEmpty()) {
            throw new MissingValuesException(newRecord.getClass(), List.of("fields", "children"));
        }
        replacements.forEach(Runnable::run);
        validator.validate(jpaRecord);
        return modelMapper.toDTO(vocabularyRecordRepository.save(jpaRecord));
    }

    @Override
    public VocabularyRecord delete(long id) {
        if (!vocabularyRecordRepository.existsById(id)) {
            throw new EntityNotFoundException(VocabularyRecordEntity.class, id);
        }
        vocabularyRecordRepository.deleteById(id);
        return null;
    }

    public List<VocabularyRecord> search(long id, String searchTerm) {
        if (!searchTerm.contains(SEARCH_QUERY_DELIMITER)) {
            return Collections.emptyList();
        }
        String[] parts = searchTerm.split(SEARCH_QUERY_DELIMITER);
        String fieldName = parts[0];
        String fieldValue = parts[1];
        return vocabularyRecordRepository.findByVocabulary_IdAndFields_FieldValues_Translations_ValueLike(id, "%" + fieldValue + "%")
                .stream()
                .filter(r -> correctFieldContainsValue(r, fieldName, fieldValue))
                .map(modelMapper::toDTO)
                .collect(Collectors.toList());
    }

    private boolean correctFieldContainsValue(VocabularyRecordEntity record, String fieldName, String fieldValue) {
        for (FieldInstanceEntity field : record.getFields()) {
            if (!field.getDefinition().getName().equals(fieldName)) {
                continue;
            }
            if (field.getFieldValues().stream()
                    .flatMap(v -> v.getTranslations().stream())
                    .anyMatch(t -> t.getValue().toLowerCase().contains(fieldValue.toLowerCase()))) {
                return true;
            }
        }
        return false;
    }
}
