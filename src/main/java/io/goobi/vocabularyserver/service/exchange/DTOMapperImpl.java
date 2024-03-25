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
import io.goobi.vocabularyserver.repositories.FieldTypeRepository;
import io.goobi.vocabularyserver.repositories.VocabularySchemaRepository;
import org.springframework.stereotype.Service;

import java.util.stream.Collectors;

@Service
public class DTOMapperImpl implements DTOMapper {
    private final FieldTypeRepository fieldTypeRepository;
    private final VocabularySchemaRepository vocabularySchemaRepository;

    public DTOMapperImpl(FieldTypeRepository fieldTypeRepository, VocabularySchemaRepository vocabularySchemaRepository) {
        this.fieldTypeRepository = fieldTypeRepository;
        this.vocabularySchemaRepository = vocabularySchemaRepository;
    }

    private VocabularySchema lookUpSchema(Long id) {
        return vocabularySchemaRepository
                .findById(id)
                .orElseThrow(() -> new EntityNotFoundException(VocabularySchema.class, id));
    }

    private FieldType lookUpFieldType(Long id) {
        return fieldTypeRepository
                .findById(id)
                .orElseThrow(() -> new EntityNotFoundException(FieldType.class, id));
    }

    @Override
    public FieldDefinition toEntity(FieldDefinitionDTO dto) {
        FieldDefinition result = new FieldDefinition(lookUpSchema(dto.getSchemaId()), dto.getName(), lookUpFieldType(dto.getTypeId()));
        result.setId(dto.getId());
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
        return null;
    }

    @Override
    public FieldInstanceDTO toDTO(FieldInstance entity) {
        return null;
    }

    @Override
    public FieldType toEntity(FieldTypeDTO dto) {
        FieldType result = new FieldType(dto.getName());
        result.setId(dto.getId());
        result.setValidation(dto.getValidation());
        if (dto.getSelectableValues() != null) {
            result.setSelectableValues(dto.getSelectableValues().stream().map(SelectableValue::new).collect(Collectors.toSet()));
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
        return null;
    }

    @Override
    public VocabularyDTO toDTO(Vocabulary entity) {
        return null;
    }

    @Override
    public VocabularySchema toEntity(VocabularySchemaDTO dto) {
        return null;
    }

    @Override
    public VocabularySchemaDTO toDTO(VocabularySchema entity) {
        return null;
    }

    @Override
    public VocabularyRecord toEntity(VocabularyRecordDTO dto) {
        return null;
    }

    @Override
    public VocabularyRecordDTO toDTO(VocabularyRecord entity) {
        return null;
    }
}
