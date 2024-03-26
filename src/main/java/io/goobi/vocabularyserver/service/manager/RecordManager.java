package io.goobi.vocabularyserver.service.manager;

import io.goobi.vocabularyserver.exception.EntityNotFoundException;
import io.goobi.vocabularyserver.exception.ValidationException;
import io.goobi.vocabularyserver.exchange.VocabularyRecordDTO;
import io.goobi.vocabularyserver.model.Vocabulary;
import io.goobi.vocabularyserver.model.VocabularyRecord;
import io.goobi.vocabularyserver.repositories.VocabularyRecordRepository;
import io.goobi.vocabularyserver.repositories.VocabularyRepository;
import io.goobi.vocabularyserver.service.exchange.DTOMapper;
import io.goobi.vocabularyserver.validation.Validator;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.stream.Collectors;

@Service
public class RecordManager {
    private final VocabularyRepository vocabularyRepository;
    private final VocabularyRecordRepository vocabularyRecordRepository;
    private final DTOMapper modelMapper;

    private final Validator<VocabularyRecord> validator;

    public RecordManager(VocabularyRepository vocabularyRepository,
                         VocabularyRecordRepository vocabularyRecordRepository, DTOMapper modelMapper, Validator<VocabularyRecord> validator) {
        this.vocabularyRepository = vocabularyRepository;
        this.vocabularyRecordRepository = vocabularyRecordRepository;
        this.modelMapper = modelMapper;
        this.validator = validator;
    }

    public List<VocabularyRecordDTO> listAll(long id) {
        Vocabulary jpaVocabulary = vocabularyRepository
                .findById(id)
                .orElseThrow(() -> new EntityNotFoundException(Vocabulary.class, id));
        return jpaVocabulary.getRecords()
                .stream()
                .map(modelMapper::toDTO)
                .collect(Collectors.toList());
    }

    public VocabularyRecordDTO get(long id) {
        return modelMapper.toDTO(
                vocabularyRecordRepository.findById(id)
                        .orElseThrow(() -> new EntityNotFoundException(VocabularyRecord.class, id))
        );
    }

    public VocabularyRecordDTO create(VocabularyRecordDTO newRecord) throws ValidationException {
        VocabularyRecord jpaVocabularyRecord = modelMapper.toEntity(newRecord);
        validator.validate(jpaVocabularyRecord);
        return modelMapper.toDTO(vocabularyRecordRepository.save(jpaVocabularyRecord));
    }

    public VocabularyRecordDTO delete(long id) {
        if (!vocabularyRecordRepository.existsById(id)) {
            throw new EntityNotFoundException(VocabularyRecord.class, id);
        }
        vocabularyRecordRepository.deleteById(id);
        return null;
    }
}
