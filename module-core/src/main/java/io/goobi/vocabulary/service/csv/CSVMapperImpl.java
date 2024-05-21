package io.goobi.vocabulary.service.csv;

import io.goobi.vocabulary.model.jpa.VocabularyEntity;
import org.apache.jena.rdf.model.Model;
import org.springframework.stereotype.Service;

@Service
public class CSVMapperImpl implements CSVMapper {
    @Override
    public String toCSV(VocabularyEntity entity) {
//        return transform(entity, this::generateVocabularyModel, RDF_TURTLE_SYNTAX);
        return "CSV";
    }

    private Model generateVocabularyModel(VocabularyEntity entity) {
//        String uri = generateURIForId(VocabularyController.class, entity.getId());
//
//        Model model = ModelFactory.createDefaultModel();
//        Map<Long, Resource> recordMap = new HashMap<>();
//
//        Resource vocabulary = model.createResource(uri)
//                .addProperty(RDF.type, SKOS.Concept);
//
//        for (VocabularyRecordEntity r : entity.getRecords()) {
//            generateRecordResource(model, recordMap, r);
//        }
//
//        // Create between record references
//        entity.getRecords().forEach(r -> generateBetweenRecordReferences(r, recordMap));
//
//        model.setNsPrefix("skos", SKOS.uri);
//
//        return model;
        return null;
    }
}
