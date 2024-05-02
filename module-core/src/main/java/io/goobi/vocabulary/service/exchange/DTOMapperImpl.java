package io.goobi.vocabulary.service.exchange;

import io.goobi.vocabulary.api.assemblers.RecordAssembler;
import io.goobi.vocabulary.exception.EntityNotFoundException;
import io.goobi.vocabulary.exception.MissingAttributeException;
import io.goobi.vocabulary.exchange.FieldDefinition;
import io.goobi.vocabulary.exchange.FieldInstance;
import io.goobi.vocabulary.exchange.FieldType;
import io.goobi.vocabulary.exchange.FieldValue;
import io.goobi.vocabulary.exchange.Language;
import io.goobi.vocabulary.exchange.TranslationDefinition;
import io.goobi.vocabulary.exchange.TranslationInstance;
import io.goobi.vocabulary.exchange.Vocabulary;
import io.goobi.vocabulary.exchange.VocabularyRecord;
import io.goobi.vocabulary.exchange.VocabularySchema;
import io.goobi.vocabulary.model.jpa.FieldDefinitionEntity;
import io.goobi.vocabulary.model.jpa.FieldInstanceEntity;
import io.goobi.vocabulary.model.jpa.FieldTranslationEntity;
import io.goobi.vocabulary.model.jpa.FieldTypeEntity;
import io.goobi.vocabulary.model.jpa.FieldValueEntity;
import io.goobi.vocabulary.model.jpa.LanguageEntity;
import io.goobi.vocabulary.model.jpa.SelectableValueEntity;
import io.goobi.vocabulary.model.jpa.TranslationDefinitionEntity;
import io.goobi.vocabulary.model.jpa.VocabularyEntity;
import io.goobi.vocabulary.model.jpa.VocabularyRecordEntity;
import io.goobi.vocabulary.model.jpa.VocabularySchemaEntity;
import io.goobi.vocabulary.repositories.FieldDefinitionRepository;
import io.goobi.vocabulary.repositories.FieldInstanceRepository;
import io.goobi.vocabulary.repositories.FieldTypeRepository;
import io.goobi.vocabulary.repositories.LanguageRepository;
import io.goobi.vocabulary.repositories.VocabularyRecordRepository;
import io.goobi.vocabulary.repositories.VocabularyRepository;
import io.goobi.vocabulary.repositories.VocabularySchemaRepository;
import org.springframework.stereotype.Service;

import java.util.Map;
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
        if (dto.getTranslationDefinitions() != null) {
            result.setTranslationDefinitions(dto.getTranslationDefinitions().stream()
                    .map(td -> toEntity(td, false))
                    .collect(Collectors.toList())
            );
            result.getTranslationDefinitions().forEach(fv -> fv.setFieldDefinition(result));
        }
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
        result.setTranslationDefinitions(entity.getTranslationDefinitions().stream()
                .map(this::toDTO)
                .collect(Collectors.toSet())
        );
        return result;
    }

    @Override
    public TranslationDefinitionEntity toEntity(TranslationDefinition dto, boolean fullInitialization) {
        TranslationDefinitionEntity result = new TranslationDefinitionEntity();
        if (dto.getId() != null) {
            result.setId(dto.getId());
        }
        if (fullInitialization) {
            if (dto.getDefinitionId() == null) {
                throw new MissingAttributeException(TranslationDefinition.class, "definitionId");
            }
            result.setFieldDefinition(lookUpFieldDefinition(dto.getDefinitionId()));
        }
        if (dto.getLanguage() == null) {
            throw new MissingAttributeException(TranslationDefinition.class, "language");
        }
        result.setLanguage(lookUpLanguage(dto.getLanguage()));
        result.setFallback(Boolean.TRUE.equals(dto.getFallback()));
        result.setRequired(Boolean.TRUE.equals(dto.getRequired()));
        return result;
    }

    @Override
    public TranslationDefinition toDTO(TranslationDefinitionEntity entity) {
        TranslationDefinition result = new TranslationDefinition();
        result.setId(entity.getId());
        result.setDefinitionId(entity.getFieldDefinition().getId());
        result.setLanguage(entity.getLanguage().getAbbreviation());
        result.setFallback(Boolean.TRUE.equals(entity.getFallback()));
        result.setRequired(entity.isRequired());
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
        result.setTranslations(dto.getTranslations().stream()
                .map(e -> toEntity(e, result))
                .collect(Collectors.toList())
        );
        return result;
    }

    private FieldTranslationEntity toEntity(TranslationInstance translationInstance, FieldValueEntity fieldValue) {
        FieldTranslationEntity result = new FieldTranslationEntity();
        // ID is not present but will be auto-generated anyway
        // Special case, "" for non-translatable values
        if ("".equals(translationInstance.getLanguage())) {
            result.setLanguage(null);
        } else {
            result.setLanguage(lookUpLanguage(translationInstance.getLanguage()));
        }
        result.setFieldValue(fieldValue);
        result.setValue(translationInstance.getValue());
        return result;
    }

    @Override
    // TODO: Test this
    public FieldValue toDTO(FieldValueEntity entity) {
        FieldValue result = new FieldValue();
        result.setId(entity.getId());
        result.setFieldId(entity.getFieldInstance().getId());
        result.setTranslations(entity.getTranslations().stream()
                        .map(t -> {
                            TranslationInstance translationInstance = new TranslationInstance();
                            translationInstance.setLanguage(t.getLanguage() != null ? t.getLanguage().getAbbreviation() : "");
                            translationInstance.setValue(t.getValue());
                            return translationInstance;
                        })
                .collect(Collectors.toList())
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
    public VocabularySchemaEntity toEntity(VocabularySchema dto) {
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
    public VocabularySchema toDTO(VocabularySchemaEntity entity) {
        VocabularySchema result = new VocabularySchema();
        result.setId(entity.getId());
        result.setDefinitions(entity.getDefinitions().stream().map(this::toDTO).collect(Collectors.toList()));
        result.setHierarchicalRecords(Boolean.TRUE.equals(entity.isHierarchicalRecords()));
        return result;
    }

    @Override
    public VocabularyRecordEntity toEntity(VocabularyRecord dto) {
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
            result.setChildren(dto.getChildren().stream()
                    .map(this::lookUpRecord)
                    .collect(Collectors.toList()));
        }
        result.getChildren().forEach(c -> c.setParentRecord(result));
        return result;
    }

    @Override
    public VocabularyRecord toDTO(VocabularyRecordEntity entity) {
        VocabularyRecord result = new VocabularyRecord();
        result.setId(entity.getId());
        if (entity.getParentRecord() != null) {
            result.setParentId(entity.getParentRecord().getId());
        }
        result.setVocabularyId(entity.getVocabulary().getId());
        result.setFields(entity.getFields().stream().map(this::toDTO).collect(Collectors.toSet()));
        result.setChildren(entity.getChildren().stream()
                .map(VocabularyRecordEntity::getId)
                .collect(Collectors.toSet()));
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
