package io.goobi.vocabularyserver.service.manager;

import io.goobi.vocabularyserver.exception.EntityNotFoundException;
import io.goobi.vocabularyserver.exchange.VocabularySchema;
import io.goobi.vocabularyserver.model.FieldDefinition;
import io.goobi.vocabularyserver.model.FieldType;
import io.goobi.vocabularyserver.repositories.FieldTypeRepository;
import io.goobi.vocabularyserver.repositories.VocabularySchemaRepository;
import io.goobi.vocabularyserver.service.exchange.ExchangeTypeTransformer;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.stream.Collectors;

@Service
public class VocabularySchemaManager implements Manager<VocabularySchema> {

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
    public List<VocabularySchema> listAll() {
        return vocabularySchemaRepository.findAll()
                .stream()
                .map(exchangeTypeTransformer::transform)
                .collect(Collectors.toList());
    }

    @Override
    public VocabularySchema get(long id) {
        return exchangeTypeTransformer.transform(
                vocabularySchemaRepository.findById(id)
                        .orElseThrow(() -> new EntityNotFoundException(io.goobi.vocabularyserver.model.VocabularySchema.class, id))
        );
    }

    @Override
    public VocabularySchema create(VocabularySchema newSchema) {
        io.goobi.vocabularyserver.model.VocabularySchema jpaSchema = transformSchema(newSchema);
        return exchangeTypeTransformer.transform(vocabularySchemaRepository.save(jpaSchema));
    }

    public io.goobi.vocabularyserver.model.VocabularySchema transformSchema(VocabularySchema newSchema) {
        io.goobi.vocabularyserver.model.VocabularySchema result = new io.goobi.vocabularyserver.model.VocabularySchema();
        result.getDefinitions()
                .addAll(newSchema.getDefinitions()
                        .stream()
                        .map(f -> transformFieldDefinition(result, f))
                        .collect(Collectors.toSet())
                );
        return result;
    }

    public FieldDefinition transformFieldDefinition(io.goobi.vocabularyserver.model.VocabularySchema schema, io.goobi.vocabularyserver.exchange.FieldDefinition fieldDefinition) {
        FieldType type = fieldTypeRepository.findById(fieldDefinition.getTypeId())
                .orElseThrow(() -> new EntityNotFoundException(FieldType.class, fieldDefinition.getTypeId()));
        io.goobi.vocabularyserver.model.FieldDefinition result = new io.goobi.vocabularyserver.model.FieldDefinition(schema, fieldDefinition.getName(), type);
        result.setMainEntry(fieldDefinition.isMainEntry());
        result.setTitleField(fieldDefinition.isTitleField());
        result.setRequired(fieldDefinition.isRequired());
        result.setUnique(fieldDefinition.isUnique());
        return result;
    }
}
