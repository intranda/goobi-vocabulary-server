package io.goobi.vocabularyserver.service.manager;

import io.goobi.vocabularyserver.exception.EntityNotFoundException;
import io.goobi.vocabularyserver.exception.RecordValidationException;
import io.goobi.vocabularyserver.exception.ValidationException;
import io.goobi.vocabularyserver.exchange.VocabularyRecordDTO;
import io.goobi.vocabularyserver.model.VocabularyRecordEntity;
import io.goobi.vocabularyserver.repositories.VocabularyRecordRepository;
import io.goobi.vocabularyserver.service.exchange.DTOMapper;
import io.goobi.vocabularyserver.validation.Validator;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;

import java.util.LinkedList;
import java.util.List;
import java.util.stream.Collectors;

@Service
public class RecordDTOManager implements Manager<VocabularyRecordDTO> {
    private final VocabularyRecordRepository vocabularyRecordRepository;
    private final DTOMapper modelMapper;
    private final Validator<VocabularyRecordEntity> validator;

    public RecordDTOManager(VocabularyRecordRepository vocabularyRecordRepository, DTOMapper modelMapper, Validator<VocabularyRecordEntity> validator) {
        this.vocabularyRecordRepository = vocabularyRecordRepository;
        this.modelMapper = modelMapper;
        this.validator = validator;
    }

    public Page<VocabularyRecordDTO> listAll(long id, Pageable pageRequest) {
        return vocabularyRecordRepository.findByVocabulary_IdAndParentRecordNull(id, pageRequest)
                .map(modelMapper::toDTO);
    }

    @Override
    public VocabularyRecordDTO get(long id) {
        return modelMapper.toDTO(
                vocabularyRecordRepository.findById(id)
                        .orElseThrow(() -> new EntityNotFoundException(VocabularyRecordEntity.class, id))
        );
    }

    @Override
    public VocabularyRecordDTO create(VocabularyRecordDTO newRecord) throws ValidationException {
        VocabularyRecordEntity jpaVocabularyRecord = modelMapper.toEntity(newRecord);
        validator.validate(jpaVocabularyRecord);
        return modelMapper.toDTO(vocabularyRecordRepository.save(jpaVocabularyRecord));
    }

    public VocabularyRecordDTO createSubRecord(VocabularyRecordDTO newRecord) throws ValidationException {
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
    public VocabularyRecordDTO delete(long id) {
        if (!vocabularyRecordRepository.existsById(id)) {
            throw new EntityNotFoundException(VocabularyRecordEntity.class, id);
        }
        vocabularyRecordRepository.deleteById(id);
        return null;
    }

    public Page<VocabularyRecordDTO> search(long id, String searchTerm, Pageable pageRequest) {
        return vocabularyRecordRepository.findByVocabulary_IdAndFields_FieldValues_Translations_ValueLike(id, searchTerm, pageRequest)
                .map(modelMapper::toDTO);
    }
}
