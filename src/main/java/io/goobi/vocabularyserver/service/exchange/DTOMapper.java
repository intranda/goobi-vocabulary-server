package io.goobi.vocabularyserver.service.exchange;

import io.goobi.vocabularyserver.exception.EntityNotFoundException;
import io.goobi.vocabularyserver.exchange.FieldDefinitionDTO;
import io.goobi.vocabularyserver.exchange.FieldTypeDTO;
import io.goobi.vocabularyserver.model.FieldDefinition;
import io.goobi.vocabularyserver.model.FieldType;
import io.goobi.vocabularyserver.model.SelectableValue;
import io.goobi.vocabularyserver.model.VocabularySchema;
import io.goobi.vocabularyserver.repositories.FieldTypeRepository;
import io.goobi.vocabularyserver.repositories.VocabularySchemaRepository;
import org.modelmapper.Converter;
import org.modelmapper.ModelMapper;
import org.modelmapper.Provider;
import org.modelmapper.convention.MatchingStrategies;
import org.modelmapper.spi.MappingContext;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

@Configuration
public class DTOMapper {
    private final FieldTypeRepository fieldTypeRepository;
    private final VocabularySchemaRepository vocabularySchemaRepository;
    private final Provider<FieldType> typeProvider = provisionRequest -> {
        FieldDefinitionDTO from = (FieldDefinitionDTO) provisionRequest.getSource();
        return lookUpFieldType(from.getTypeId());
    };

    public DTOMapper(FieldTypeRepository fieldTypeRepository, VocabularySchemaRepository vocabularySchemaRepository) {
        this.fieldTypeRepository = fieldTypeRepository;
        this.vocabularySchemaRepository = vocabularySchemaRepository;
    }

    private final Converter<Set<SelectableValue>, Set<String>> selectableValuesConverter = mapping -> {
        if (mapping.getSource() == null) {
            return null;
        }
        return mapping.getSource().stream()
                .map(SelectableValue::getValue)
                .collect(Collectors.toSet());
    };

    private final Converter<Set<String>, Set<SelectableValue>> selectableValuesDTOConverter = mapping -> {
        if (mapping.getSource() == null) {
            return null;
        }
        return mapping.getSource().stream()
                .map(SelectableValue::new)
                .collect(Collectors.toSet());
    };

    private final Converter<VocabularySchema, Long> schemaToIdConverter = mapping ->
            mapping.getSource().getId();
    private final Converter<FieldType, Long> fieldTypeToIdConverter = mapping ->
            mapping.getSource().getId();

    private final Converter<Long, VocabularySchema> schemaLookUpConverter = mapping ->
            lookUpSchema(mapping.getSource());
    private final Converter<Long, FieldType> fieldTypeLookUpConverter = new Converter<Long, FieldType>() {
        @Override
        public FieldType convert(MappingContext<Long, FieldType> mappingContext) {
            System.err.println("HERE");
            return lookUpFieldType(mappingContext.getSource());
        }
    };

    private final Converter<List<FieldDefinitionDTO>, List<FieldDefinition>> fieldDefinitionsConverter = mapping ->
            mapping.getSource().stream()
                    .map(this::transformFieldDefinition)
                    .collect(Collectors.toList());

    private final Converter<FieldDefinition, Long> fieldDefinitionConverter = mapping ->
            mapping.getSource().getSchema().getId();

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

    private FieldDefinition transformFieldDefinition(FieldDefinitionDTO fieldDefinitionDTO) {
        FieldType type = fieldTypeRepository.findById(fieldDefinitionDTO.getTypeId())
                .orElseThrow(() -> new EntityNotFoundException(FieldType.class, fieldDefinitionDTO.getTypeId()));
//        VocabularySchema schema = vocabularySchemaRepository.findById(fieldDefinitionDTO.getSchemaId())
//                .orElseThrow(() -> new EntityNotFoundException(VocabularySchema.class, fieldDefinitionDTO.getSchemaId()));
        FieldDefinition result = new FieldDefinition(null, fieldDefinitionDTO.getName(), type);
        result.setMainEntry(fieldDefinitionDTO.isMainEntry());
        result.setTitleField(fieldDefinitionDTO.isTitleField());
        result.setRequired(fieldDefinitionDTO.isRequired());
        result.setUnique(fieldDefinitionDTO.isUnique());
        return result;
    }

    @Bean
    public ModelMapper modelMapper() {
        ModelMapper mapper = new ModelMapper();

        mapper.getConfiguration().setMatchingStrategy(MatchingStrategies.STRICT);
        mapper.getConfiguration().setFullTypeMatchingRequired(true);

        mapper.getConfiguration().setProvider(typeProvider);
//        mapper.getConfiguration().setPreferNestedProperties(false);
//        mapper.getConfiguration().setAmbiguityIgnored(true);

//        mapper.getConfiguration().setImplicitMappingEnabled(false);

        mapper.typeMap(FieldType.class, FieldTypeDTO.class)
                .addMappings(m -> m.using(selectableValuesConverter).map(FieldType::getSelectableValues, FieldTypeDTO::setSelectableValues));
        mapper.typeMap(FieldTypeDTO.class, FieldType.class)
                .addMappings(m -> m.using(selectableValuesDTOConverter).map(FieldTypeDTO::getSelectableValues, FieldType::setSelectableValues));

        mapper.typeMap(FieldDefinition.class, FieldDefinitionDTO.class)
                .addMappings(m -> m.using(schemaToIdConverter).map(FieldDefinition::getSchema, FieldDefinitionDTO::setSchemaId))
                .addMappings(m -> m.using(fieldTypeToIdConverter).map(FieldDefinition::getType, FieldDefinitionDTO::setTypeId));
        mapper.createTypeMap(FieldDefinitionDTO.class, FieldDefinition.class)
                .addMappings(m -> m.using(schemaLookUpConverter).map(FieldDefinitionDTO::getSchemaId, FieldDefinition::setSchema))
                .addMappings(m -> m.map(FieldDefinitionDTO::getTypeId, FieldDefinition::setType));

//        mapper.typeMap(VocabularyDTO.class, Vocabulary.class)
//                .addMappings(m -> m.using(schemaDtoConverter).map(VocabularyDTO::getSchemaId, Vocabulary::setSchema))
//                .addMappings(m -> m.skip(Vocabulary::setSchema));

//        mapper.typeMap(VocabularySchemaDTO.class, VocabularySchema.class)
//                .addMappings(m -> m.using(fieldDefinitionsConverter).map(VocabularySchemaDTO::getDefinitions, VocabularySchema::setDefinitions));


        mapper.validate();

        return mapper;
    }
}
