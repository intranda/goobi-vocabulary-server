package io.goobi.vocabularyserver.service.manager;

import io.goobi.vocabularyserver.exception.EntityNotFoundException;
import io.goobi.vocabularyserver.exception.ValidationException;
import io.goobi.vocabularyserver.exchange.VocabularyRecordDTO;
import io.goobi.vocabularyserver.model.VocabularyRecord;
import io.goobi.vocabularyserver.repositories.VocabularyRecordRepository;
import io.goobi.vocabularyserver.service.exchange.DTOMapper;
import io.goobi.vocabularyserver.validation.Validator;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.stream.Collectors;

@Service
public class RecordManager {
    private final VocabularyRecordRepository vocabularyRecordRepository;
    private final DTOMapper modelMapper;

    private final Validator<VocabularyRecord> validator;

    public RecordManager(VocabularyRecordRepository vocabularyRecordRepository, DTOMapper modelMapper, Validator<VocabularyRecord> validator) {
        this.vocabularyRecordRepository = vocabularyRecordRepository;
        this.modelMapper = modelMapper;
        this.validator = validator;
    }

    public Page<VocabularyRecordDTO> listAll(long id, Pageable pageRequest) {
        return vocabularyRecordRepository.findByVocabulary_Id(id, pageRequest)
                .map(modelMapper::toDTO);
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

    public List<VocabularyRecordDTO> search(long id, String searchTerm) {
        return vocabularyRecordRepository.findByVocabulary_IdAndFields_ValueLikeIgnoreCase(id, searchTerm)
                .stream()
                .map(modelMapper::toDTO)
                .collect(Collectors.toList());
    }
}
