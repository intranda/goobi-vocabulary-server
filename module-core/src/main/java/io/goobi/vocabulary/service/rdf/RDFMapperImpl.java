package io.goobi.vocabulary.service.rdf;

import io.goobi.vocabulary.api.LanguageController;
import io.goobi.vocabulary.api.VocabularyController;
import io.goobi.vocabulary.api.VocabularyRecordController;
import io.goobi.vocabulary.exception.EntityNotFoundException;
import io.goobi.vocabulary.exception.MappingException;
import io.goobi.vocabulary.model.jpa.FieldDefinitionEntity;
import io.goobi.vocabulary.model.jpa.FieldInstanceEntity;
import io.goobi.vocabulary.model.jpa.FieldTranslationEntity;
import io.goobi.vocabulary.model.jpa.FieldTypeEntity;
import io.goobi.vocabulary.model.jpa.FieldValueEntity;
import io.goobi.vocabulary.model.jpa.LanguageEntity;
import io.goobi.vocabulary.model.jpa.VocabularyEntity;
import io.goobi.vocabulary.model.jpa.VocabularyRecordEntity;
import io.goobi.vocabulary.service.rdf.vocabulary.LANGUAGE;
import org.apache.jena.datatypes.xsd.XSDDatatype;
import org.apache.jena.rdf.model.Model;
import org.apache.jena.rdf.model.ModelFactory;
import org.apache.jena.rdf.model.Property;
import org.apache.jena.rdf.model.RDFNode;
import org.apache.jena.rdf.model.Resource;
import org.apache.jena.riot.RDFDataMgr;
import org.apache.jena.riot.RDFFormat;
import org.apache.jena.vocabulary.DCTerms;
import org.apache.jena.vocabulary.RDF;
import org.apache.jena.vocabulary.SKOS;
import org.apache.jena.vocabulary.XSD;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;

import java.io.StringWriter;
import java.lang.reflect.Method;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;

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
    public boolean isRDFCompatible(VocabularyEntity entity) {
        try {
            // TODO: Think about how to export / resolve referenced entries
            if (entity.getSchema().getDefinitions().stream()
                    .map(FieldDefinitionEntity::getType)
                    .anyMatch(Objects::isNull)) {
                return false;
            }
            // TODO: Metadata schema validation
            if (entity.getMetadataSchema() == null) {
                return false;
            }
            entity.getSchema().getDefinitions().stream()
                    .map(FieldDefinitionEntity::getType)
                    .filter(Objects::nonNull)
                    .map(FieldTypeEntity::getName)
                    .forEach(this::findSkosProperty);
            return true;
        } catch (IllegalArgumentException e) {
            return false;
        }
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
        Model model = ModelFactory.createDefaultModel();
        Map<Long, Resource> recordMap = new HashMap<>();

        // Should be only one entry
        VocabularyRecordEntity metadata = entity.getRecords().stream()
                .filter(VocabularyRecordEntity::isMetadata)
                .findFirst()
                .orElseThrow(() -> new EntityNotFoundException(VocabularyRecordEntity.class, "metadata"));
        Resource conceptScheme = generateConceptSchemeResource(model, recordMap, metadata);

        entity.getRecords().stream()
                .filter(r -> !r.isMetadata())
                .forEachOrdered(r -> generateRecordResource(model, recordMap, r));

        // Create between record references
        entity.getRecords().stream()
                .filter(r -> !r.isMetadata())
                .forEach(r -> generateBetweenRecordReferences(r, recordMap));

        // Create top concept references
        entity.getRecords().stream()
                .filter(r -> !r.isMetadata())
                .filter(r -> r.getParentRecord() == null)
                .forEach(r -> generateTopConceptReferences(conceptScheme, r, recordMap));

        // SKOS:inScheme attribute for singleRootElement vocabularies
        createSkosInSchemePropertyIfApplicable(model, entity.getRecords(), recordMap);

        model.setNsPrefix("skos", SKOS.uri);
        model.setNsPrefix("dct", DCTerms.NS);
        model.setNsPrefix("xsd", XSD.NS);

        return model;
    }

    private Resource generateConceptSchemeResource(Model model, Map<Long, Resource> recordMap, VocabularyRecordEntity record) {
        Resource result = model.createResource(generateURIForId(VocabularyController.class, record.getVocabulary().getId()))
                .addProperty(RDF.type, SKOS.ConceptScheme);
        recordMap.put(record.getId(), result);

        record.getFields().forEach(f -> processField(model, f, result));

        // No child processing required

        return result;
    }

    private Resource generateRecordResource(Model model, Map<Long, Resource> recordMap, VocabularyRecordEntity record) {
        Resource result = model.createResource(generateURIForId(VocabularyRecordController.class, record.getId()))
                .addProperty(RDF.type, SKOS.Concept);
        recordMap.put(record.getId(), result);

        record.getFields().forEach(f -> processField(model, f, result));

        for (VocabularyRecordEntity child : record.getChildren()) {
            generateRecordResource(model, recordMap, child);
        }

        return result;
    }

    private void processField(Model model, FieldInstanceEntity field, Resource record) {
        for (FieldValueEntity value : field.getFieldValues()) {
            String type = field.getDefinition().getType().getName();
            Property property = findSkosProperty(type);
            for (FieldTranslationEntity t : value.getTranslations()) {
                record.addProperty(property, generatePropertyDependingNode(model, property, t));
            }
        }
    }

    private RDFNode generatePropertyDependingNode(Model model, Property property, FieldTranslationEntity t) {
        if (property.equals(DCTerms.license) || property.equals(SKOS.closeMatch) || property.equals(SKOS.exactMatch)) {
            return model.createResource(t.getValue());
        } else if (property.equals(DCTerms.created)) {
            return model.createTypedLiteral(t.getValue(), XSDDatatype.XSDdate);
        } else {
            return generateTranslatedNode(model, t);
        }
    }

    private Property findSkosProperty(String type) {
        return switch (type) {
            case "skos:prefLabel" -> SKOS.prefLabel;
            case "skos:altLabel" -> SKOS.altLabel;
            case "skos:definition" -> SKOS.definition;
            case "skos:editorialNote" -> SKOS.editorialNote;
            case "skos:closeMatch" -> SKOS.closeMatch;
            case "skos:exactMatch" -> SKOS.exactMatch;
            case "dct:title" -> DCTerms.title;
            case "dct:creator" -> DCTerms.creator;
            case "dct:created" -> DCTerms.created;
            case "dct:license" -> DCTerms.license;
            default -> throw new IllegalArgumentException("Unknown SKOS/DCT type \"" + type + "\"");
        };
    }

    private RDFNode generateTranslatedNode(Model model, FieldTranslationEntity t) {
        if (t.getLanguage() == null) {
            return model.createLiteral(t.getValue());
        }
        return model.createLiteral(t.getValue(), transformToTwoCharacterLanguageIdentifier(t.getLanguage().getAbbreviation()));
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

    private void generateTopConceptReferences(Resource conceptScheme, VocabularyRecordEntity r, Map<Long, Resource> recordMap) {
        Resource recordResource = recordMap.get(r.getId());
        conceptScheme.addProperty(SKOS.hasTopConcept, recordResource);
        recordResource.addProperty(SKOS.topConceptOf, conceptScheme);
    }

    private void createSkosInSchemePropertyIfApplicable(Model model, List<VocabularyRecordEntity> records, Map<Long, Resource> recordMap) {
        List<VocabularyRecordEntity> topRecords = records.stream()
                .filter(r -> !r.isMetadata() && r.getParentRecord() == null)
                .toList();
        if (topRecords.size() != 1) {
            return;
        }
        VocabularyRecordEntity topRecord = topRecords.getFirst();
        long topRecordId = topRecord.getId();
        Resource topElement = recordMap.get(topRecord.getId());

        // Find all other records and add the property
        records.stream()
                .filter(r -> !r.isMetadata() && r.getId() != topRecordId)
                .forEach(r -> recordMap.get(r.getId()).addProperty(SKOS.inScheme, topElement));
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
