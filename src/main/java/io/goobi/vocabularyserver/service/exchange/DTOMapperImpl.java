package io.goobi.vocabularyserver.service.exchange;

import io.goobi.vocabularyserver.api.assemblers.RecordAssembler;
import io.goobi.vocabularyserver.exception.EntityNotFoundException;
import io.goobi.vocabularyserver.exception.MissingAttributeException;
import io.goobi.vocabularyserver.exchange.FieldDefinition;
import io.goobi.vocabularyserver.exchange.FieldInstance;
import io.goobi.vocabularyserver.exchange.FieldType;
import io.goobi.vocabularyserver.exchange.FieldValue;
import io.goobi.vocabularyserver.exchange.Language;
import io.goobi.vocabularyserver.exchange.Vocabulary;
import io.goobi.vocabularyserver.exchange.VocabularyRecordDTO;
import io.goobi.vocabularyserver.exchange.VocabularySchemaDTO;
import io.goobi.vocabularyserver.model.FieldDefinitionEntity;
import io.goobi.vocabularyserver.model.FieldInstanceEntity;
import io.goobi.vocabularyserver.model.FieldTranslationEntity;
import io.goobi.vocabularyserver.model.FieldTypeEntity;
import io.goobi.vocabularyserver.model.FieldValueEntity;
import io.goobi.vocabularyserver.model.LanguageEntity;
import io.goobi.vocabularyserver.model.SelectableValueEntity;
import io.goobi.vocabularyserver.model.VocabularyEntity;
import io.goobi.vocabularyserver.model.VocabularyRecordEntity;
import io.goobi.vocabularyserver.model.VocabularySchemaEntity;
import io.goobi.vocabularyserver.repositories.FieldDefinitionRepository;
import io.goobi.vocabularyserver.repositories.FieldInstanceRepository;
import io.goobi.vocabularyserver.repositories.FieldTypeRepository;
import io.goobi.vocabularyserver.repositories.LanguageRepository;
import io.goobi.vocabularyserver.repositories.VocabularyRecordRepository;
import io.goobi.vocabularyserver.repositories.VocabularyRepository;
import io.goobi.vocabularyserver.repositories.VocabularySchemaRepository;
import org.springframework.stereotype.Service;

import java.util.Map;
import java.util.Objects;
import java.util.stream.Collectors;

@Service
public class DTOMapperImpl implements DTOMapper {
    private final RecordAssembler recordAssembler;
    private final FieldTypeRepository fieldTypeRepository;
    private final VocabularySchemaRepository vocabularySchemaRepository;
    private final VocabularyRecordRepository vocabularyRecordRepository;
    private final VocabularyRepository vocabularyRepository;
    private final FieldDefinitionRepository fieldDefinitionRepository;
    private final FieldInstanceRepository fieldInstanceRepository;
    private final LanguageRepository languageRepository;

    public DTOMapperImpl(RecordAssembler recordAssembler, FieldTypeRepository fieldTypeRepository, VocabularySchemaRepository vocabularySchemaRepository,
                         VocabularyRecordRepository vocabularyRecordRepository,
                         VocabularyRepository vocabularyRepository,
                         FieldDefinitionRepository fieldDefinitionRepository,
                         FieldInstanceRepository fieldInstanceRepository,
                         LanguageRepository languageRepository) {
        this.recordAssembler = recordAssembler;
        this.fieldTypeRepository = fieldTypeRepository;
        this.vocabularySchemaRepository = vocabularySchemaRepository;
        this.vocabularyRecordRepository = vocabularyRecordRepository;
        this.vocabularyRepository = vocabularyRepository;
        this.fieldDefinitionRepository = fieldDefinitionRepository;
        this.fieldInstanceRepository = fieldInstanceRepository;
        this.languageRepository = languageRepository;
    }

    private VocabularyEntity lookUpVocabulary(Long id) {
        return vocabularyRepository
                .findById(id)
                .orElseThrow(() -> new EntityNotFoundException(VocabularyEntity.class, id));
    }

    private VocabularySchemaEntity lookUpSchema(Long id) {
        return vocabularySchemaRepository
                .findById(id)
                .orElseThrow(() -> new EntityNotFoundException(VocabularySchemaEntity.class, id));
    }

    private VocabularyRecordEntity lookUpRecord(Long id) {
        return vocabularyRecordRepository
                .findById(id)
                .orElseThrow(() -> new EntityNotFoundException(VocabularyRecordEntity.class, id));
    }

    private FieldDefinitionEntity lookUpFieldDefinition(Long id) {
        return fieldDefinitionRepository
                .findById(id)
                .orElseThrow(() -> new EntityNotFoundException(FieldDefinitionEntity.class, id));
    }

    private FieldInstanceEntity lookupFieldInstance(Long id) {
        return fieldInstanceRepository
                .findById(id)
                .orElseThrow(() -> new EntityNotFoundException(FieldInstanceEntity.class, id));
    }

    private FieldTypeEntity lookUpFieldType(Long id) {
        return fieldTypeRepository
                .findById(id)
                .orElseThrow(() -> new EntityNotFoundException(FieldTypeEntity.class, id));
    }

    private LanguageEntity lookUpLanguage(String abbreviation) {
        return languageRepository.findByAbbreviation(abbreviation)
                .orElseThrow(() -> new EntityNotFoundException(LanguageEntity.class, abbreviation));
    }

    @Override
    public FieldDefinitionEntity toEntity(FieldDefinition dto, boolean fullInitialization) {
        FieldDefinitionEntity result = new FieldDefinitionEntity();
        if (dto.getId() != null) {
            result.setId(dto.getId());
        }
        if (fullInitialization) {
            if (dto.getSchemaId() == null) {
                throw new MissingAttributeException(FieldInstanceEntity.class, "schemaId");
            }
            result.setSchema(lookUpSchema(dto.getSchemaId()));
        }
        if (dto.getTypeId() == null) {
            throw new MissingAttributeException(FieldInstanceEntity.class, "typeId");
        }
        result.setType(lookUpFieldType(dto.getTypeId()));
        result.setName(dto.getName());
        result.setRequired(Boolean.TRUE.equals(dto.getRequired()));
        result.setUnique(Boolean.TRUE.equals(dto.getUnique()));
        result.setMainEntry(Boolean.TRUE.equals(dto.getMainEntry()));
        result.setTitleField(Boolean.TRUE.equals(dto.getTitleField()));
        result.setMultiValued(Boolean.TRUE.equals(dto.getMultiValued()));
        return result;
    }

    @Override
    public FieldDefinition toDTO(FieldDefinitionEntity entity) {
        FieldDefinition result = new FieldDefinition();
        result.setId(entity.getId());
        result.setSchemaId(entity.getSchema().getId());
        result.setName(entity.getName());
        result.setTypeId(entity.getType().getId());
        result.setRequired(entity.isRequired());
        result.setUnique(entity.isUnique());
        result.setMainEntry(Boolean.TRUE.equals(entity.getMainEntry()));
        result.setTitleField(entity.isTitleField());
        result.setMultiValued(entity.isMultiValued());
        return result;
    }

    @Override
    public FieldInstanceEntity toEntity(FieldInstance dto, boolean fullInitialization) {
        FieldInstanceEntity result = new FieldInstanceEntity();
        if (dto.getId() != null) {
            result.setId(dto.getId());
        }
        if (fullInitialization) {
            if (dto.getRecordId() == null) {
                throw new MissingAttributeException(FieldInstanceEntity.class, "recordId");
            }
            result.setVocabularyRecord(lookUpRecord(dto.getRecordId()));
        }
        if (dto.getDefinitionId() == null) {
            throw new MissingAttributeException(FieldInstanceEntity.class, "definitionId");
        }
        result.setDefinition(lookUpFieldDefinition(dto.getDefinitionId()));
        result.setFieldValues(dto.getValues().stream()
                .map(fv -> toEntity(fv, false))
                .collect(Collectors.toList())
        );
        result.getFieldValues().forEach(fv -> fv.setFieldInstance(result));
        return result;
    }

    @Override
    public FieldInstance toDTO(FieldInstanceEntity entity) {
        FieldInstance result = new FieldInstance();
        result.setId(entity.getId());
        result.setRecordId(entity.getVocabularyRecord().getId());
        result.setDefinitionId(entity.getDefinition().getId());
        result.setValues(entity.getFieldValues().stream()
                .map(this::toDTO)
                .collect(Collectors.toSet())
        );
        return result;
    }

    @Override
    // TODO: Test this
    public FieldValueEntity toEntity(FieldValue dto, boolean fullInitialization) {
        FieldValueEntity result = new FieldValueEntity();
        if (dto.getId() != null) {
            result.setId(dto.getId());
        }
        // TODO: Maybe manual initialization
        if (fullInitialization) {
            if (dto.getFieldId() == null) {
                throw new MissingAttributeException(FieldInstanceEntity.class, "fieldId");
            }
            result.setFieldInstance(lookupFieldInstance(dto.getFieldId()));
        }
        // TODO: Maybe issue with same IDs
        result.setTranslations(dto.getTranslations().entrySet().stream()
                .map(e -> toEntity(e, result))
                .collect(Collectors.toList())
        );
        return result;
    }

    private FieldTranslationEntity toEntity(Map.Entry<String, String> entry, FieldValueEntity fieldValue) {
        FieldTranslationEntity result = new FieldTranslationEntity();
        // ID is not present but will be auto-generated anyway
        result.setLanguage(lookUpLanguage(entry.getKey()));
        result.setFieldValue(fieldValue);
        result.setValue(entry.getValue());
        return result;
    }

    @Override
    // TODO: Test this
    public FieldValue toDTO(FieldValueEntity entity) {
        FieldValue result = new FieldValue();
        result.setId(entity.getId());
        result.setFieldId(entity.getFieldInstance().getId());
        result.setTranslations(entity.getTranslations().stream()
                .collect(Collectors.toMap(t -> t.getLanguage().getAbbreviation(), FieldTranslationEntity::getValue))
        );
        return result;
    }

    @Override
    public FieldTypeEntity toEntity(FieldType dto) {
        FieldTypeEntity result = new FieldTypeEntity();
        if (dto.getId() != null) {
            result.setId(dto.getId());
        }
        result.setName(dto.getName());
        result.setValidation(dto.getValidation());
        if (dto.getSelectableValues() != null) {
            result.setSelectableValues(dto.getSelectableValues().stream()
                    .map(s -> {
                                SelectableValueEntity sv = new SelectableValueEntity();
                                sv.setFieldType(result);
                                sv.setValue(s);
                                return sv;
                            }
                    ).collect(Collectors.toList()));
        }
        return result;
    }

    @Override
    public FieldType toDTO(FieldTypeEntity entity) {
        FieldType result = new FieldType();
        result.setId(entity.getId());
        result.setName(entity.getName());
        result.setValidation(entity.getValidation());
        if (entity.getSelectableValues() != null) {
            result.setSelectableValues(entity.getSelectableValues().stream().map(SelectableValueEntity::getValue).collect(Collectors.toSet()));
        }
        return result;
    }

    @Override
    public VocabularyEntity toEntity(Vocabulary dto) {
        VocabularyEntity result = new VocabularyEntity();
        if (dto.getId() != null) {
            result.setId(dto.getId());
        }
        if (dto.getSchemaId() == null) {
            throw new MissingAttributeException(FieldInstanceEntity.class, "schemaId");
        }
        result.setSchema(lookUpSchema(dto.getSchemaId()));
        result.setName(dto.getName());
        result.setDescription(dto.getDescription());
        return result;
    }

    @Override
    public Vocabulary toDTO(VocabularyEntity entity) {
        Vocabulary result = new Vocabulary();
        result.setId(entity.getId());
        result.setSchemaId(entity.getSchema().getId());
        result.setName(entity.getName());
        result.setDescription(entity.getDescription());
        return result;
    }

    @Override
    public VocabularySchemaEntity toEntity(VocabularySchemaDTO dto) {
        VocabularySchemaEntity result = new VocabularySchemaEntity();
        if (dto.getId() != null) {
            result.setId(dto.getId());
        }
        result.setDefinitions(dto.getDefinitions().stream()
                .map(d -> this.toEntity(d, false)
                ).collect(Collectors.toList()));
        result.getDefinitions().forEach(d -> d.setSchema(result));
        result.setHierarchicalRecords(Boolean.TRUE.equals(dto.getHierarchicalRecords()));
        return result;
    }

    @Override
    public VocabularySchemaDTO toDTO(VocabularySchemaEntity entity) {
        VocabularySchemaDTO result = new VocabularySchemaDTO();
        result.setId(entity.getId());
        result.setDefinitions(entity.getDefinitions().stream().map(this::toDTO).collect(Collectors.toList()));
        result.setHierarchicalRecords(Boolean.TRUE.equals(entity.isHierarchicalRecords()));
        return result;
    }

    @Override
    public VocabularyRecordEntity toEntity(VocabularyRecordDTO dto) {
        VocabularyRecordEntity result = new VocabularyRecordEntity();
        if (dto.getId() != null) {
            result.setId(dto.getId());
        }
        if (dto.getVocabularyId() == null) {
            throw new MissingAttributeException(FieldInstanceEntity.class, "vocabularyId");
        }
        result.setVocabulary(lookUpVocabulary(dto.getVocabularyId()));
        result.setFields(dto.getFields().stream()
                .map(f -> this.toEntity(f, false))
                .collect(Collectors.toList()));
        result.getFields().forEach(f -> f.setVocabularyRecord(result));
        if (dto.getChildren() != null) {
            result.setChildren(dto.getChildren().stream().map(c -> toEntity(Objects.requireNonNull(c.getContent()))).collect(Collectors.toList()));
        }
        result.getChildren().forEach(c -> c.setParentRecord(result));
        return result;
    }

    @Override
    public VocabularyRecordDTO toDTO(VocabularyRecordEntity entity) {
        VocabularyRecordDTO result = new VocabularyRecordDTO();
        result.setId(entity.getId());
        if (entity.getParentRecord() != null) {
            result.setParentId(entity.getParentRecord().getId());
        }
        result.setVocabularyId(entity.getVocabulary().getId());
        result.setFields(entity.getFields().stream().map(this::toDTO).collect(Collectors.toSet()));
        result.setChildren(entity.getChildren().stream().map(c -> recordAssembler.toModel(toDTO(c))).collect(Collectors.toSet()));
        result.getChildren().forEach(c -> Objects.requireNonNull(c.getContent()).setParentId(result.getId()));
        if (result.getChildren().isEmpty()) {
            result.setChildren(null);
        }
        return result;
    }

    @Override
    public LanguageEntity toEntity(Language dto) {
        LanguageEntity result = new LanguageEntity();
        if (dto.getId() != null) {
            result.setId(dto.getId());
        }
        result.setAbbreviation(dto.getAbbreviation());
        result.setName(dto.getName());
        return result;
    }

    @Override
    public Language toDTO(LanguageEntity entity) {
        Language result = new Language();
        result.setId(entity.getId());
        result.setAbbreviation(entity.getAbbreviation());
        result.setName(entity.getName());
        return result;
    }
}
