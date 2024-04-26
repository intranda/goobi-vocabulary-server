package io.goobi.vocabulary.service.manager;

import io.goobi.vocabulary.exception.EntityNotFoundException;
import io.goobi.vocabulary.exception.RecordValidationException;
import io.goobi.vocabulary.exception.ValidationException;
import io.goobi.vocabulary.exchange.VocabularyRecord;
import io.goobi.vocabulary.model.jpa.VocabularyRecordEntity;
import io.goobi.vocabulary.repositories.VocabularyRecordRepository;
import io.goobi.vocabulary.service.exchange.DTOMapper;
import io.goobi.vocabulary.validation.Validator;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;

import java.util.LinkedList;
import java.util.List;
import java.util.stream.Collectors;

@Service
public class RecordDTOManager implements Manager<VocabularyRecord> {
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
    public VocabularyRecord delete(long id) {
        if (!vocabularyRecordRepository.existsById(id)) {
            throw new EntityNotFoundException(VocabularyRecordEntity.class, id);
        }
        vocabularyRecordRepository.deleteById(id);
        return null;
    }

    public Page<VocabularyRecord> search(long id, String searchTerm, Pageable pageRequest) {
        return vocabularyRecordRepository.findByVocabulary_IdAndFields_FieldValues_Translations_ValueLike(id, searchTerm, pageRequest)
                .map(modelMapper::toDTO);
    }
}
