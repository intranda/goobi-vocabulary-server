package io.goobi.vocabularyserver.service.exchange;

import io.goobi.vocabularyserver.exception.EntityNotFoundException;
import io.goobi.vocabularyserver.exchange.FieldDefinitionDTO;
import io.goobi.vocabularyserver.exchange.FieldInstanceDTO;
import io.goobi.vocabularyserver.exchange.FieldTypeDTO;
import io.goobi.vocabularyserver.exchange.FieldValueDTO;
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

import java.util.Iterator;
import java.util.Map;
import java.util.stream.Collectors;

@Service
public class DTOMapperImpl implements DTOMapper {
    private final FieldTypeRepository fieldTypeRepository;
    private final VocabularySchemaRepository vocabularySchemaRepository;
    private final VocabularyRecordRepository vocabularyRecordRepository;
    private final VocabularyRepository vocabularyRepository;
    private final FieldDefinitionRepository fieldDefinitionRepository;
    private final FieldInstanceRepository fieldInstanceRepository;
    private final LanguageRepository languageRepository;

    public DTOMapperImpl(FieldTypeRepository fieldTypeRepository, VocabularySchemaRepository vocabularySchemaRepository,
                         VocabularyRecordRepository vocabularyRecordRepository,
                         VocabularyRepository vocabularyRepository,
                         FieldDefinitionRepository fieldDefinitionRepository,
                         FieldInstanceRepository fieldInstanceRepository,
                         LanguageRepository languageRepository) {
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
        result.setId(dto.getId());
        if (fullInitialization) {
            result.setSchema(lookUpSchema(dto.getSchemaId()));
        }
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
    public FieldInstance toEntity(FieldInstanceDTO dto, boolean fullInitialization) {
        FieldInstance result = new FieldInstance();
        result.setId(dto.getId());
        if (fullInitialization) {
            result.setVocabularyRecord(lookUpRecord(dto.getRecordId()));
        }
        result.setDefinition(lookUpFieldDefinition(dto.getDefinitionId()));
        result.setFieldValues(dto.getValues().stream()
                .map(fv -> toEntity(fv, false))
                .collect(Collectors.toSet())
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
        result.setId(dto.getId());
        // TODO: Maybe manual initialization
        if (fullInitialization) {
            result.setFieldInstance(lookupFieldInstance(dto.getFieldId()));
        }
        result.setTranslations(dto.getTranslations().entrySet().stream()
                .map(e -> toEntity(e, result))
                .collect(Collectors.toSet())
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
        result.setDefinitions(dto.getDefinitions().stream()
                .map(d -> this.toEntity(d, false)
                ).collect(Collectors.toList()));
        result.getDefinitions().forEach(d -> d.setSchema(result));
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
        // Field instance equality is based on IDs, therefore we need to provide distince IDs for all field instances.
        // Otherwise, after collecting them in sets will result in only one field.
        // The IDs are ignored by JPA anyway.
        Iterator<FieldInstanceDTO> fieldIterator = dto.getFields().iterator();
        for (int i = 1; i <= dto.getFields().size(); i++) {
            fieldIterator.next().setId(i);
        }
        result.setFields(dto.getFields().stream()
                .map(f -> this.toEntity(f, false))
                .collect(Collectors.toSet()));
        result.getFields().forEach(f -> f.setVocabularyRecord(result));
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
