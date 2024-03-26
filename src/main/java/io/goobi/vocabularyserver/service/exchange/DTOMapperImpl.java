package io.goobi.vocabularyserver.service.exchange;

import io.goobi.vocabularyserver.exception.EntityNotFoundException;
import io.goobi.vocabularyserver.exchange.FieldDefinitionDTO;
import io.goobi.vocabularyserver.exchange.FieldInstanceDTO;
import io.goobi.vocabularyserver.exchange.FieldTypeDTO;
import io.goobi.vocabularyserver.exchange.VocabularyDTO;
import io.goobi.vocabularyserver.exchange.VocabularyRecordDTO;
import io.goobi.vocabularyserver.exchange.VocabularySchemaDTO;
import io.goobi.vocabularyserver.model.FieldDefinition;
import io.goobi.vocabularyserver.model.FieldInstance;
import io.goobi.vocabularyserver.model.FieldType;
import io.goobi.vocabularyserver.model.SelectableValue;
import io.goobi.vocabularyserver.model.Vocabulary;
import io.goobi.vocabularyserver.model.VocabularyRecord;
import io.goobi.vocabularyserver.model.VocabularySchema;
import io.goobi.vocabularyserver.repositories.FieldDefinitionRepository;
import io.goobi.vocabularyserver.repositories.FieldTypeRepository;
import io.goobi.vocabularyserver.repositories.VocabularyRecordRepository;
import io.goobi.vocabularyserver.repositories.VocabularyRepository;
import io.goobi.vocabularyserver.repositories.VocabularySchemaRepository;
import org.springframework.stereotype.Service;

import java.util.stream.Collectors;

@Service
public class DTOMapperImpl implements DTOMapper {
    private final FieldTypeRepository fieldTypeRepository;
    private final VocabularySchemaRepository vocabularySchemaRepository;
    private final VocabularyRecordRepository vocabularyRecordRepository;
    private final VocabularyRepository vocabularyRepository;
    private final FieldDefinitionRepository fieldDefinitionRepository;

    public DTOMapperImpl(FieldTypeRepository fieldTypeRepository, VocabularySchemaRepository vocabularySchemaRepository,
                         VocabularyRecordRepository vocabularyRecordRepository,
                         VocabularyRepository vocabularyRepository,
                         FieldDefinitionRepository fieldDefinitionRepository) {
        this.fieldTypeRepository = fieldTypeRepository;
        this.vocabularySchemaRepository = vocabularySchemaRepository;
        this.vocabularyRecordRepository = vocabularyRecordRepository;
        this.vocabularyRepository = vocabularyRepository;
        this.fieldDefinitionRepository = fieldDefinitionRepository;
    }

    private Vocabulary lookUpVocabulary(Long id) {
        return vocabularyRepository
                .findById(id)
                .orElseThrow(() -> new EntityNotFoundException(Vocabulary.class, id));
    }

    private VocabularySchema lookUpSchema(Long id) {
        return vocabularySchemaRepository
                .findById(id)
                .orElseThrow(() -> new EntityNotFoundException(VocabularySchema.class, id));
    }

    private VocabularyRecord lookUpRecord(Long id) {
        return vocabularyRecordRepository
                .findById(id)
                .orElseThrow(() -> new EntityNotFoundException(VocabularyRecord.class, id));
    }

    private FieldDefinition lookUpFieldDefinition(Long id) {
        return fieldDefinitionRepository
                .findById(id)
                .orElseThrow(() -> new EntityNotFoundException(FieldDefinition.class, id));
    }

    private FieldType lookUpFieldType(Long id) {
        return fieldTypeRepository
                .findById(id)
                .orElseThrow(() -> new EntityNotFoundException(FieldType.class, id));
    }

    @Override
    public FieldDefinition toEntity(FieldDefinitionDTO dto) {
        FieldDefinition result = new FieldDefinition();
        result.setId(dto.getId());
        result.setSchema(lookUpSchema(dto.getSchemaId()));
        result.setType(lookUpFieldType(dto.getTypeId()));
        result.setName(dto.getName());
        result.setRequired(dto.isRequired());
        result.setUnique(dto.isUnique());
        result.setMainEntry(dto.isMainEntry());
        result.setTitleField(dto.isTitleField());
        return result;
    }

    @Override
    public FieldDefinitionDTO toDTO(FieldDefinition entity) {
        FieldDefinitionDTO result = new FieldDefinitionDTO();
        result.setId(entity.getId());
        result.setSchemaId(entity.getSchema().getId());
        result.setName(entity.getName());
        result.setTypeId(entity.getType().getId());
        result.setRequired(entity.getRequired());
        result.setUnique(entity.getUnique());
        result.setMainEntry(entity.getMainEntry() != null);
        result.setTitleField(entity.getTitleField());
        return result;
    }

    @Override
    public FieldInstance toEntity(FieldInstanceDTO dto) {
        FieldInstance result = new FieldInstance();
        result.setId(dto.getId());
        result.setVocabularyRecord(lookUpRecord(dto.getRecordId()));
        result.setDefinition(lookUpFieldDefinition(dto.getDefinitionId()));
        result.setLanguage(dto.getLanguage());
        result.setValue(dto.getValue());
        return result;
    }

    @Override
    public FieldInstanceDTO toDTO(FieldInstance entity) {
        FieldInstanceDTO result = new FieldInstanceDTO();
        result.setId(entity.getId());
        result.setRecordId(entity.getVocabularyRecord().getId());
        result.setDefinitionId(entity.getDefinition().getId());
        result.setLanguage(entity.getLanguage());
        result.setValue(entity.getValue());
        return result;
    }

    @Override
    public FieldType toEntity(FieldTypeDTO dto) {
        FieldType result = new FieldType();
        result.setId(dto.getId());
        result.setName(dto.getName());
        result.setValidation(dto.getValidation());
        if (dto.getSelectableValues() != null) {
            result.setSelectableValues(dto.getSelectableValues().stream()
                    .map(s -> {
                                SelectableValue sv = new SelectableValue();
                                sv.setValue(s);
                                return sv;
                            }
                    ).collect(Collectors.toSet()));
        }
        return result;
    }

    @Override
    public FieldTypeDTO toDTO(FieldType entity) {
        FieldTypeDTO result = new FieldTypeDTO();
        result.setId(entity.getId());
        result.setName(entity.getName());
        result.setValidation(entity.getValidation());
        if (entity.getSelectableValues() != null) {
            result.setSelectableValues(entity.getSelectableValues().stream().map(SelectableValue::getValue).collect(Collectors.toSet()));
        }
        return result;
    }

    @Override
    public Vocabulary toEntity(VocabularyDTO dto) {
        Vocabulary result = new Vocabulary();
        result.setId(dto.getId());
        result.setSchema(lookUpSchema(dto.getSchemaId()));
        result.setName(dto.getName());
        result.setDescription(dto.getDescription());
        return result;
    }

    @Override
    public VocabularyDTO toDTO(Vocabulary entity) {
        VocabularyDTO result = new VocabularyDTO();
        result.setId(entity.getId());
        result.setSchemaId(entity.getSchema().getId());
        result.setName(entity.getName());
        result.setDescription(entity.getDescription());
        return result;
    }

    @Override
    public VocabularySchema toEntity(VocabularySchemaDTO dto) {
        VocabularySchema result = new VocabularySchema();
        result.setId(dto.getId());
        result.setDefinitions(dto.getDefinitions().stream().map(this::toEntity).collect(Collectors.toList()));
        return result;
    }

    @Override
    public VocabularySchemaDTO toDTO(VocabularySchema entity) {
        VocabularySchemaDTO result = new VocabularySchemaDTO();
        result.setId(entity.getId());
        result.setDefinitions(entity.getDefinitions().stream().map(this::toDTO).collect(Collectors.toList()));
        return result;
    }

    @Override
    public VocabularyRecord toEntity(VocabularyRecordDTO dto) {
        VocabularyRecord result = new VocabularyRecord();
        result.setId(dto.getId());
        result.setVocabulary(lookUpVocabulary(dto.getVocabularyId()));
        result.setFields(dto.getFields().stream().map(this::toEntity).collect(Collectors.toSet()));
        return result;
    }

    @Override
    public VocabularyRecordDTO toDTO(VocabularyRecord entity) {
        VocabularyRecordDTO result = new VocabularyRecordDTO();
        result.setId(entity.getId());
        result.setVocabularyId(entity.getVocabulary().getId());
        result.setFields(entity.getFields().stream().map(this::toDTO).collect(Collectors.toSet()));
        return result;
    }
}
