package io.goobi.vocabularyserver.service.exchange;

import io.goobi.vocabularyserver.api.assemblers.RecordAssembler;
import io.goobi.vocabularyserver.exception.EntityNotFoundException;
import io.goobi.vocabularyserver.exchange.FieldDefinitionDTO;
import io.goobi.vocabularyserver.exchange.FieldInstanceDTO;
import io.goobi.vocabularyserver.exchange.FieldTypeDTO;
import io.goobi.vocabularyserver.exchange.FieldValueDTO;
import io.goobi.vocabularyserver.exchange.LanguageDTO;
import io.goobi.vocabularyserver.exchange.VocabularyDTO;
import io.goobi.vocabularyserver.exchange.VocabularyRecordDTO;
import io.goobi.vocabularyserver.exchange.VocabularySchemaDTO;
import io.goobi.vocabularyserver.model.FieldDefinition;
import io.goobi.vocabularyserver.model.FieldInstance;
import io.goobi.vocabularyserver.model.FieldTranslation;
import io.goobi.vocabularyserver.model.FieldType;
import io.goobi.vocabularyserver.model.FieldValue;
import io.goobi.vocabularyserver.model.Language;
import io.goobi.vocabularyserver.model.SelectableValue;
import io.goobi.vocabularyserver.model.Vocabulary;
import io.goobi.vocabularyserver.model.VocabularyRecord;
import io.goobi.vocabularyserver.model.VocabularySchema;
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

    private FieldInstance lookupFieldInstance(Long id) {
        return fieldInstanceRepository
                .findById(id)
                .orElseThrow(() -> new EntityNotFoundException(FieldInstance.class, id));
    }

    private FieldType lookUpFieldType(Long id) {
        return fieldTypeRepository
                .findById(id)
                .orElseThrow(() -> new EntityNotFoundException(FieldType.class, id));
    }

    private Language lookUpLanguage(String abbreviation) {
        return languageRepository.findByAbbreviation(abbreviation)
                .orElseThrow(() -> new EntityNotFoundException(Language.class, abbreviation));
    }

    @Override
    public FieldDefinition toEntity(FieldDefinitionDTO dto, boolean fullInitialization) {
        FieldDefinition result = new FieldDefinition();
        if (dto.getId() != null) {
            result.setId(dto.getId());
        }
        if (fullInitialization) {
            result.setSchema(lookUpSchema(dto.getSchemaId()));
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
    public FieldDefinitionDTO toDTO(FieldDefinition entity) {
        FieldDefinitionDTO result = new FieldDefinitionDTO();
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
    public FieldInstance toEntity(FieldInstanceDTO dto, boolean fullInitialization) {
        FieldInstance result = new FieldInstance();
        if (dto.getId() != null) {
            result.setId(dto.getId());
        }
        if (fullInitialization) {
            result.setVocabularyRecord(lookUpRecord(dto.getRecordId()));
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
    public FieldInstanceDTO toDTO(FieldInstance entity) {
        FieldInstanceDTO result = new FieldInstanceDTO();
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
    public FieldValue toEntity(FieldValueDTO dto, boolean fullInitialization) {
        FieldValue result = new FieldValue();
        if (dto.getId() != null) {
            result.setId(dto.getId());
        }
        // TODO: Maybe manual initialization
        if (fullInitialization) {
            result.setFieldInstance(lookupFieldInstance(dto.getFieldId()));
        }
        // TODO: Maybe issue with same IDs
        result.setTranslations(dto.getTranslations().entrySet().stream()
                .map(e -> toEntity(e, result))
                .collect(Collectors.toList())
        );
        return result;
    }

    private FieldTranslation toEntity(Map.Entry<String, String> entry, FieldValue fieldValue) {
        FieldTranslation result = new FieldTranslation();
        // ID is not present but will be auto-generated anyway
        result.setLanguage(lookUpLanguage(entry.getKey()));
        result.setFieldValue(fieldValue);
        result.setValue(entry.getValue());
        return result;
    }

    @Override
    // TODO: Test this
    public FieldValueDTO toDTO(FieldValue entity) {
        FieldValueDTO result = new FieldValueDTO();
        result.setId(entity.getId());
        result.setFieldId(entity.getFieldInstance().getId());
        result.setTranslations(entity.getTranslations().stream()
                .collect(Collectors.toMap(t -> t.getLanguage().getAbbreviation(), FieldTranslation::getValue))
        );
        return result;
    }

    @Override
    public FieldType toEntity(FieldTypeDTO dto) {
        FieldType result = new FieldType();
        if (dto.getId() != null) {
            result.setId(dto.getId());
        }
        result.setName(dto.getName());
        result.setValidation(dto.getValidation());
        if (dto.getSelectableValues() != null) {
            result.setSelectableValues(dto.getSelectableValues().stream()
                    .map(s -> {
                                SelectableValue sv = new SelectableValue();
                                sv.setFieldType(result);
                                sv.setValue(s);
                                return sv;
                            }
                    ).collect(Collectors.toList()));
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
        if (dto.getId() != null) {
            result.setId(dto.getId());
        }
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
    public VocabularySchemaDTO toDTO(VocabularySchema entity) {
        VocabularySchemaDTO result = new VocabularySchemaDTO();
        result.setId(entity.getId());
        result.setDefinitions(entity.getDefinitions().stream().map(this::toDTO).collect(Collectors.toList()));
        result.setHierarchicalRecords(Boolean.TRUE.equals(entity.isHierarchicalRecords()));
        return result;
    }

    @Override
    public VocabularyRecord toEntity(VocabularyRecordDTO dto) {
        VocabularyRecord result = new VocabularyRecord();
        if (dto.getId() != null) {
            result.setId(dto.getId());
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
    public VocabularyRecordDTO toDTO(VocabularyRecord entity) {
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
    public Language toEntity(LanguageDTO dto) {
        Language result = new Language();
        if (dto.getId() != null) {
            result.setId(dto.getId());
        }
        result.setAbbreviation(dto.getAbbreviation());
        result.setName(dto.getName());
        return result;
    }

    @Override
    public LanguageDTO toDTO(Language entity) {
        LanguageDTO result = new LanguageDTO();
        result.setId(entity.getId());
        result.setAbbreviation(entity.getAbbreviation());
        result.setName(entity.getName());
        return result;
    }
}
