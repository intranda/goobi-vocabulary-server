package io.goobi.vocabulary.service.rdf;

import io.goobi.vocabulary.api.LanguageController;
import io.goobi.vocabulary.api.VocabularyController;
import io.goobi.vocabulary.api.VocabularyRecordController;
import io.goobi.vocabulary.exception.MappingException;
import io.goobi.vocabulary.model.jpa.FieldInstanceEntity;
import io.goobi.vocabulary.model.jpa.FieldTranslationEntity;
import io.goobi.vocabulary.model.jpa.FieldValueEntity;
import io.goobi.vocabulary.model.jpa.LanguageEntity;
import io.goobi.vocabulary.model.jpa.VocabularyEntity;
import io.goobi.vocabulary.model.jpa.VocabularyRecordEntity;
import io.goobi.vocabulary.service.rdf.vocabulary.LANGUAGE;
import org.apache.jena.rdf.model.Model;
import org.apache.jena.rdf.model.ModelFactory;
import org.apache.jena.rdf.model.Property;
import org.apache.jena.rdf.model.Resource;
import org.apache.jena.riot.RDFDataMgr;
import org.apache.jena.riot.RDFFormat;
import org.apache.jena.vocabulary.RDF;
import org.apache.jena.vocabulary.SKOS;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;

import java.io.StringWriter;
import java.lang.reflect.Method;
import java.util.HashMap;
import java.util.Map;

@Service
public class RDFMapperImpl implements RDFMapper {
    @Value("${vocabulary-server.base-url}")
    private String host;

    @Value("${server.port}")
    private int port;

    public static final RDFFormat RDF_XML_SYNTAX = RDFFormat.RDFXML;
    public static final RDFFormat RDF_TURTLE_SYNTAX = RDFFormat.TURTLE_BLOCKS;

    private <T> String transform(T entity, EntityToModelMappingFunction<T> function, RDFFormat format) {
        StringWriter out = new StringWriter();
        RDFDataMgr.write(out, function.map(entity), format);
        return out.toString();
    }

    private String generateURIForId(Class<?> clazz, long id) {
        try {
            String classRoute = extractClassEndpoint(clazz);
            String methodRoute = extractMethodEndpoint(clazz.getMethod("one", long.class));
            String endpoint = classRoute + methodRoute.replace("{id}", Long.toString(id));
            return host + ':' + port + endpoint;
        } catch (NoSuchMethodException e) {
            throw new MappingException(clazz, String.class, e);
        }
    }

    private static String extractClassEndpoint(Class<?> clazz) throws NoSuchMethodException {
        String[] values = clazz.getAnnotation(RequestMapping.class).value();
        assert (values.length == 1);
        return values[0];
    }

    private static String extractMethodEndpoint(Method method) throws NoSuchMethodException {
        String[] values = method.getAnnotation(GetMapping.class).value();
        assert (values.length == 1);
        return values[0];
    }

    @Override
    public String toRDFXML(LanguageEntity entity) {
        return transform(entity, this::generateLanguageModel, RDF_XML_SYNTAX);
    }

    @Override
    public String toRDFXML(VocabularyEntity entity) {
        return transform(entity, this::generateVocabularyModel, RDF_XML_SYNTAX);
    }

    @Override
    public String toRDFTurtle(LanguageEntity entity) {
        return transform(entity, this::generateLanguageModel, RDF_TURTLE_SYNTAX);
    }

    @Override
    public String toRDFTurtle(VocabularyEntity entity) {
        return transform(entity, this::generateVocabularyModel, RDF_TURTLE_SYNTAX);
    }

    private Model generateVocabularyModel(VocabularyEntity entity) {
        String uri = generateURIForId(VocabularyController.class, entity.getId());

        Model model = ModelFactory.createDefaultModel();
        Map<Long, Resource> recordMap = new HashMap<>();

        Resource vocabulary = model.createResource(uri)
                .addProperty(RDF.type, SKOS.Concept);

        for (VocabularyRecordEntity r : entity.getRecords()) {
            generateRecordResource(model, recordMap, r);
        }

        // Create between record references
        entity.getRecords().forEach(r -> generateBetweenRecordReferences(r, recordMap));

        model.setNsPrefix("skos", SKOS.uri);

        return model;
    }

    private Resource generateRecordResource(Model model, Map<Long, Resource> recordMap, VocabularyRecordEntity record) {
        Resource result = model.createResource(generateURIForId(VocabularyRecordController.class, record.getId()))
                .addProperty(RDF.type, SKOS.Concept);
        recordMap.put(record.getId(), result);

        record.getFields().forEach(f -> processField(f, result));

        for (VocabularyRecordEntity child : record.getChildren()) {
            generateRecordResource(model, recordMap, child);
        }

        return result;
    }

    private void processField(FieldInstanceEntity field, Resource record) {
        for (FieldValueEntity value : field.getFieldValues()) {
            String type = field.getDefinition().getType().getName();
            Property property = findSkosProperty(type);
            for (FieldTranslationEntity t : value.getTranslations()) {
                record.addProperty(property, t.getValue(), transformToTwoCharacterLanguageIdentifier(t.getLanguage().getAbbreviation()));
            }
        }
    }

    private Property findSkosProperty(String type) {
        return switch (type) {
            case "skos:prefLabel" -> SKOS.prefLabel;
            case "skos:altLabel" -> SKOS.altLabel;
            case "skos:definition" -> SKOS.definition;
            case "skos:editorialNote" -> SKOS.editorialNote;
            default -> throw new IllegalArgumentException("Unknown SKOS type \"" + type + "\"");
        };
    }

    private String transformToTwoCharacterLanguageIdentifier(String abbreviation) {
        switch (abbreviation) {
            case "eng":
                return "en";
            case "ger":
                return "de";
            case "fre":
                return "fr";
            default:
                throw new IllegalArgumentException("Unknown language identifier \"" + abbreviation + "\"");
        }
    }

    private void generateBetweenRecordReferences(VocabularyRecordEntity record, Map<Long, Resource> recordMap) {
        Resource current = recordMap.get(record.getId());
        if (record.getParentRecord() != null) {
            current.addProperty(SKOS.broader, recordMap.get(record.getParentRecord().getId()));
        }
        for (VocabularyRecordEntity child : record.getChildren()) {
            current.addProperty(SKOS.narrower, recordMap.get(child.getId()));
        }
        for (VocabularyRecordEntity child : record.getChildren()) {
            generateBetweenRecordReferences(child, recordMap);
        }
    }

    private Model generateLanguageModel(LanguageEntity entity) {
        String uri = generateURIForId(LanguageController.class, entity.getId());

        Model model = ModelFactory.createDefaultModel();

        Resource resource = model.createResource(uri)
                .addProperty(LANGUAGE.NAME, entity.getName())
                .addProperty(LANGUAGE.ABBREVIATION, entity.getAbbreviation());

        return model;
    }
}
