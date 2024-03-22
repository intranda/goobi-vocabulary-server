package io.goobi.vocabularyserver.service.manager;

import io.goobi.vocabularyserver.exception.EntityNotFoundException;
import io.goobi.vocabularyserver.exchange.FieldDefinitionDTO;
import io.goobi.vocabularyserver.exchange.VocabularySchemaDTO;
import io.goobi.vocabularyserver.model.FieldDefinition;
import io.goobi.vocabularyserver.model.FieldType;
import io.goobi.vocabularyserver.model.VocabularySchema;
import io.goobi.vocabularyserver.repositories.FieldTypeRepository;
import io.goobi.vocabularyserver.repositories.VocabularySchemaRepository;
import io.goobi.vocabularyserver.service.exchange.ExchangeTypeTransformer;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.stream.Collectors;

@Service
public class VocabularySchemaManager implements Manager<VocabularySchemaDTO> {

    private final VocabularySchemaRepository vocabularySchemaRepository;
    private final ExchangeTypeTransformer exchangeTypeTransformer;
    private final FieldTypeRepository fieldTypeRepository;

    public VocabularySchemaManager(VocabularySchemaRepository vocabularySchemaRepository, ExchangeTypeTransformer exchangeTypeTransformer,
                                   FieldTypeRepository fieldTypeRepository) {
        this.vocabularySchemaRepository = vocabularySchemaRepository;
        this.exchangeTypeTransformer = exchangeTypeTransformer;
        this.fieldTypeRepository = fieldTypeRepository;
    }

    @Override
    public List<VocabularySchemaDTO> listAll() {
        return vocabularySchemaRepository.findAll()
                .stream()
                .map(exchangeTypeTransformer::transform)
                .collect(Collectors.toList());
    }

    @Override
    public VocabularySchemaDTO get(long id) {
        return exchangeTypeTransformer.transform(
                vocabularySchemaRepository.findById(id)
                        .orElseThrow(() -> new EntityNotFoundException(VocabularySchema.class, id))
        );
    }

    @Override
    public VocabularySchemaDTO create(VocabularySchemaDTO newSchema) {
        VocabularySchema jpaSchema = transformSchema(newSchema);
        return exchangeTypeTransformer.transform(vocabularySchemaRepository.save(jpaSchema));
    }

    public VocabularySchema transformSchema(VocabularySchemaDTO newSchema) {
        VocabularySchema result = new VocabularySchema();
        result.getDefinitions()
                .addAll(newSchema.getDefinitions()
                        .stream()
                        .map(f -> transformFieldDefinition(result, f))
                        .collect(Collectors.toSet())
                );
        return result;
    }

    public FieldDefinition transformFieldDefinition(VocabularySchema schema, FieldDefinitionDTO fieldDefinitionDTO) {
        FieldType type = fieldTypeRepository.findById(fieldDefinitionDTO.getTypeId())
                .orElseThrow(() -> new EntityNotFoundException(FieldType.class, fieldDefinitionDTO.getTypeId()));
        FieldDefinition result = new FieldDefinition(schema, fieldDefinitionDTO.getName(), type);
        result.setMainEntry(fieldDefinitionDTO.isMainEntry());
        result.setTitleField(fieldDefinitionDTO.isTitleField());
        result.setRequired(fieldDefinitionDTO.isRequired());
        result.setUnique(fieldDefinitionDTO.isUnique());
        return result;
    }
}
