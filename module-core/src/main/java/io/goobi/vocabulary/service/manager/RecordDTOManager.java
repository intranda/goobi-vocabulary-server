package io.goobi.vocabulary.service.manager;

import io.goobi.vocabulary.exception.DeletionOfReferencedRecordException;
import io.goobi.vocabulary.exception.EntityNotFoundException;
import io.goobi.vocabulary.exception.MissingAttributeException;
import io.goobi.vocabulary.exception.UnsupportedEntityReplacementException;
import io.goobi.vocabulary.exception.VocabularyException;
import io.goobi.vocabulary.exchange.VocabularyRecord;
import io.goobi.vocabulary.model.jpa.FieldDefinitionEntity;
import io.goobi.vocabulary.model.jpa.FieldTranslationEntity;
import io.goobi.vocabulary.model.jpa.VocabularyEntity;
import io.goobi.vocabulary.model.jpa.VocabularyRecordEntity;
import io.goobi.vocabulary.repositories.VocabularyRecordRepository;
import io.goobi.vocabulary.repositories.VocabularyRepository;
import io.goobi.vocabulary.repositories.VocabularySchemaRepository;
import io.goobi.vocabulary.service.exchange.DTOMapper;
import io.goobi.vocabulary.validation.Validator;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.stereotype.Service;

import java.util.Collection;
import java.util.Collections;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

import static io.goobi.vocabulary.exception.VocabularyException.ErrorCode.RecordValidation;

@Service
public class RecordDTOManager implements Manager<VocabularyRecord> {
    public static final String SEARCH_QUERY_DELIMITER = ":";
    private final VocabularyRecordRepository vocabularyRecordRepository;
    private final DTOMapper modelMapper;
    private final Validator<VocabularyRecordEntity> validator;
    private final VocabularyRepository vocabularyRepository;

    public RecordDTOManager(VocabularyRecordRepository vocabularyRecordRepository, DTOMapper modelMapper, Validator<VocabularyRecordEntity> validator, VocabularyRepository vocabularyRepository, VocabularySchemaRepository vocabularySchemaRepository) {
        this.vocabularyRecordRepository = vocabularyRecordRepository;
        this.modelMapper = modelMapper;
        this.validator = validator;
        this.vocabularyRepository = vocabularyRepository;
    }

    public Page<VocabularyRecord> list(long id, Pageable pageRequest) {
        if (pageRequest.getSort().isUnsorted()) {
            return vocabularyRecordRepository.findByVocabulary_IdAndParentRecordNullAndMetadataFalse(id, pageRequest)
                    .map(modelMapper::toDTO);
        } else {
            Sort.Order order = pageRequest.getSort().stream().toList().get(0);
            Pageable cleanedUp;
            try {
                cleanedUp = PageRequest.of(pageRequest.getPageNumber(), pageRequest.getPageSize());
            } catch (UnsupportedOperationException e) {
                // Pageable might have been unpaged
                cleanedUp = Pageable.unpaged();
            }
            if (order.isAscending()) {
                return vocabularyRecordRepository.findByVocabulary_sortedByFieldDefinitionASC(id, Long.parseLong(order.getProperty()), cleanedUp)
                        .map(modelMapper::toDTO);
            } else {
                return vocabularyRecordRepository.findByVocabulary_sortedByFieldDefinitionDESC(id, Long.parseLong(order.getProperty()), cleanedUp)
                        .map(modelMapper::toDTO);
            }
        }
    }

    @Override
    public VocabularyRecord get(long id) {
        return modelMapper.toDTO(
                vocabularyRecordRepository.findById(id)
                        .orElseThrow(() -> new EntityNotFoundException(VocabularyRecordEntity.class, id))
        );
    }

    public boolean exists(long id) {
        return vocabularyRecordRepository.existsById(id);
    }

    @Override
    public VocabularyRecord create(VocabularyRecord newRecord) throws VocabularyException {
        VocabularyRecordEntity jpaVocabularyRecord = modelMapper.toEntity(newRecord);
        validator.validate(jpaVocabularyRecord);
        return modelMapper.toDTO(vocabularyRecordRepository.save(jpaVocabularyRecord));
    }

    public VocabularyRecord createSubRecord(VocabularyRecord newRecord) throws VocabularyException {
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

    private void recursiveRecordValidation(VocabularyRecordEntity recordToValidate) throws VocabularyException {
        List<VocabularyException> errors = new LinkedList<>();
        try {
            validator.validate(recordToValidate);
        } catch (VocabularyException e) {
            errors.add(e);
        }
        for (VocabularyRecordEntity r : recordToValidate.getChildren()) {
            try {
                validator.validate(r);
            } catch (VocabularyException e) {
                errors.add(e);
            }
        }
        if (!errors.isEmpty()) {
            throw new VocabularyException(RecordValidation, errors, Map.of(
                    "recordId", String.valueOf(recordToValidate.getId())
            ), (params) -> "Error validating record [" + params.get("recordId") + "]");
        }
    }

    @Override
    public VocabularyRecord replace(VocabularyRecord newRecord) throws VocabularyException {
        VocabularyRecordEntity jpaRecord = vocabularyRecordRepository
                .findById(newRecord.getId())
                .orElseThrow(() -> new UnsupportedEntityReplacementException(newRecord.getClass(), newRecord.getId()));

        // TODO: Check why the following line is required in metadata but not here
//        newRecord.getFields().forEach(f -> f.setRecordId(jpaRecord.getId()));

        List<Runnable> replacements = new LinkedList<>();
        if (!newRecord.getFields().isEmpty()) {
            replacements.add(() -> jpaRecord.getFields().clear());
            newRecord.getFields().stream()
                    .map(modelMapper::toEntity)
                    .forEach(f -> replacements.add(() -> jpaRecord.getFields().add(f)));
        }
        // TODO: Plan children replacement
        if (replacements.isEmpty()) {
            throw new MissingAttributeException(newRecord.getClass(), List.of("fields", "children"));
        }
        replacements.forEach(Runnable::run);
        validator.validate(jpaRecord);
        return modelMapper.toDTO(vocabularyRecordRepository.save(jpaRecord));
    }

    @Override
    public VocabularyRecord delete(long id) {
        VocabularyRecordEntity rec = vocabularyRecordRepository.findById(id).orElseThrow(() -> new EntityNotFoundException(VocabularyRecordEntity.class, id));
        checkForExistingReferencesToSingleVocabularyRecord(rec);
        vocabularyRecordRepository.deleteById(rec.getId());
        return null;
    }

    public Collection<VocabularyRecord> deleteAllRecords(long vocabularyId) {
        VocabularyEntity vocabulary = vocabularyRepository.findById(vocabularyId)
                .orElseThrow(() -> new EntityNotFoundException(VocabularyEntity.class, vocabularyId));
        checkForExistingReferencesInWholeVocabulary(vocabulary);
        vocabulary.getRecords().removeIf(r -> !r.isMetadata());
        vocabularyRepository.save(vocabulary);
        return Collections.emptyList();
    }

    private void checkForExistingReferencesToSingleVocabularyRecord(VocabularyRecordEntity recordEntity) {
        List<VocabularyException> errors = checkForExistingReferencesToRecords(extractAllRecordsToCheckForReferences(recordEntity));
        if (!errors.isEmpty()) {
            throw new DeletionOfReferencedRecordException(recordEntity, errors);
        }
    }

    private void checkForExistingReferencesInWholeVocabulary(VocabularyEntity vocabulary) {
        List<VocabularyException> errors = checkForExistingReferencesToRecords(
                vocabulary.getRecords().stream()
                        .flatMap(r -> extractAllRecordsToCheckForReferences(r).stream())
                        .collect(Collectors.toList()));
        if (!errors.isEmpty()) {
            throw new DeletionOfReferencedRecordException(vocabulary, errors);
        }
    }

    private List<VocabularyRecordEntity> extractAllRecordsToCheckForReferences(VocabularyRecordEntity rec) {
        List<VocabularyRecordEntity> result = new LinkedList<>();
        result.add(rec);
        rec.getChildren().forEach(c -> result.addAll(extractAllRecordsToCheckForReferences(c)));
        return result;
    }

    private List<VocabularyException> checkForExistingReferencesToRecords(Collection<VocabularyRecordEntity> records) {
        List<VocabularyException> errors = new LinkedList<>();
        for (VocabularyRecordEntity r : records) {
            try {
                recordReferenceValidation(r);
            } catch (VocabularyException e) {
                errors.add(e);
            }
        }
        return errors;
    }

    private void recordReferenceValidation(VocabularyRecordEntity r) {
        VocabularyEntity vocabulary = r.getVocabulary();
        Set<VocabularyEntity> referencingVocabularies = vocabularyRepository.findDistinctBySchema_Definitions_ReferenceVocabulary(vocabulary);
        if (referencingVocabularies.isEmpty()) {
            return;
        }
        List<VocabularyRecordEntity> referencingRecords = new LinkedList<>();
        for (VocabularyEntity vocab : referencingVocabularies) {
            Set<FieldDefinitionEntity> referencingFields = vocab.getSchema().getDefinitions().stream()
                    .filter(d -> d.getReferenceVocabulary() != null && vocabulary.getId() == d.getReferenceVocabulary().getId())
                    .collect(Collectors.toSet());
            referencingRecords.addAll(vocab.getRecords().stream()
                    .filter(rec -> rec.getFields().stream()
                            .filter(f -> referencingFields.contains(f.getDefinition()))
                            .flatMap(f -> f.getFieldValues().stream())
                            .flatMap(v -> v.getTranslations().stream())
                            .map(FieldTranslationEntity::getValue)
                            .anyMatch(v -> v.equals(String.valueOf(r.getId()))))
                    .collect(Collectors.toSet()));
        }
        if (!referencingRecords.isEmpty()) {
            throw new DeletionOfReferencedRecordException(r, referencingRecords);
        }
    }

    public Page<VocabularyRecord> search(long id, String searchTerm, Pageable pageRequest) {
        if (!searchTerm.contains(SEARCH_QUERY_DELIMITER)) {
            if (pageRequest.getSort().isUnsorted()) {
                return vocabularyRecordRepository.findRecordsInVocabulary(id, "%" + searchTerm + "%", pageRequest)
                        .map(modelMapper::toDTO);
            } else {
                Sort.Order order = pageRequest.getSort().stream().toList().get(0);
                PageRequest cleanedUp = PageRequest.of(pageRequest.getPageNumber(), pageRequest.getPageSize());
                if (order.isAscending()) {
                    return vocabularyRecordRepository.findRecordsInVocabularySortedASC(id, "%" + searchTerm + "%", Long.parseLong(order.getProperty()), cleanedUp)
                            .map(modelMapper::toDTO);
                } else {
                    return vocabularyRecordRepository.findRecordsInVocabularySortedDESC(id, "%" + searchTerm + "%", Long.parseLong(order.getProperty()), cleanedUp)
                            .map(modelMapper::toDTO);
                }
            }
        } else {
            String[] parts = searchTerm.split(SEARCH_QUERY_DELIMITER);
            long fieldId = Long.parseLong(parts[0]);
            String fieldValue = parts[1];
            return vocabularyRecordRepository.findRecordsInVocabularyByField(id, fieldId, "%" + fieldValue + "%", pageRequest)
                    .map(modelMapper::toDTO);
        }
    }

    public VocabularyRecord getMetadata(long vocabularyId) {
        return modelMapper.toDTO(
                vocabularyRecordRepository.findByVocabulary_IdAndMetadataTrue(vocabularyId)
                        .orElseThrow(() -> new EntityNotFoundException(VocabularyRecordEntity.class))
        );
    }

    public VocabularyRecord replaceMetadata(VocabularyRecord newRecord) throws VocabularyException {
        VocabularyRecordEntity jpaRecord = vocabularyRecordRepository
                .findByVocabulary_IdAndMetadataTrue(newRecord.getVocabularyId())
                .orElseThrow(() -> new UnsupportedEntityReplacementException(newRecord.getClass(), newRecord.getId()));

        newRecord.getFields().forEach(f -> f.setRecordId(jpaRecord.getId()));

        List<Runnable> replacements = new LinkedList<>();
        if (!newRecord.getFields().isEmpty()) {
            replacements.add(() -> jpaRecord.getFields().clear());
            newRecord.getFields().stream()
                    .map(modelMapper::toEntity)
                    .forEach(f -> replacements.add(() -> jpaRecord.getFields().add(f)));
        }
        // TODO: Plan children replacement
        if (replacements.isEmpty()) {
            throw new MissingAttributeException(newRecord.getClass(), List.of("fields"));
        }
        replacements.forEach(Runnable::run);
        validator.validate(jpaRecord);
        return modelMapper.toDTO(vocabularyRecordRepository.save(jpaRecord));
    }
}
