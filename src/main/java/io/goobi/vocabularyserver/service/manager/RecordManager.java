package io.goobi.vocabularyserver.service.manager;

import io.goobi.vocabularyserver.exception.EntityNotFoundException;
import io.goobi.vocabularyserver.exchange.VocabularyRecordDTO;
import io.goobi.vocabularyserver.model.Vocabulary;
import io.goobi.vocabularyserver.model.VocabularyRecord;
import io.goobi.vocabularyserver.repositories.VocabularyRecordRepository;
import io.goobi.vocabularyserver.repositories.VocabularyRepository;
import org.modelmapper.ModelMapper;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.stream.Collectors;

@Service
public class RecordManager {
    private final VocabularyRepository vocabularyRepository;
    private final VocabularyRecordRepository vocabularyRecordRepository;
    private final ModelMapper modelMapper;

    public RecordManager(VocabularyRepository vocabularyRepository,
                         VocabularyRecordRepository vocabularyRecordRepository, ModelMapper modelMapper) {
        this.vocabularyRepository = vocabularyRepository;
        this.vocabularyRecordRepository = vocabularyRecordRepository;
        this.modelMapper = modelMapper;
    }

    public List<VocabularyRecordDTO> listAll(long id) {
        Vocabulary jpaVocabulary = vocabularyRepository
                .findById(id)
                .orElseThrow(() -> new EntityNotFoundException(Vocabulary.class, id));
        return jpaVocabulary.getRecords()
                .stream()
                .map(r -> modelMapper.map(r, VocabularyRecordDTO.class))
                .collect(Collectors.toList());
    }

    public VocabularyRecordDTO get(long id) {
        return modelMapper.map(
                vocabularyRecordRepository.findById(id)
                        .orElseThrow(() -> new EntityNotFoundException(VocabularyRecord.class, id)),
                VocabularyRecordDTO.class
        );
    }

    public VocabularyRecordDTO create(VocabularyRecordDTO newRecord) {
        VocabularyRecord jpaVocabularyRecord = modelMapper.map(newRecord, VocabularyRecord.class);
        return modelMapper.map(vocabularyRecordRepository.save(jpaVocabularyRecord), VocabularyRecordDTO.class);
    }

    public VocabularyRecordDTO delete(long id) {
        if (!vocabularyRecordRepository.existsById(id)) {
            throw new EntityNotFoundException(VocabularyRecord.class, id);
        }
        vocabularyRecordRepository.deleteById(id);
        return null;
    }
}
