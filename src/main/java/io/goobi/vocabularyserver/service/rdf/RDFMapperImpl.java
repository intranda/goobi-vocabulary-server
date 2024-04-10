package io.goobi.vocabularyserver.service.rdf;

import io.goobi.vocabularyserver.api.LanguageController;
import io.goobi.vocabularyserver.exception.MappingException;
import io.goobi.vocabularyserver.model.LanguageEntity;
import io.goobi.vocabularyserver.service.rdf.vocabulary.LANGUAGE;
import org.apache.jena.rdf.model.Model;
import org.apache.jena.rdf.model.ModelFactory;
import org.apache.jena.rdf.model.Resource;
import org.apache.jena.riot.RDFDataMgr;
import org.apache.jena.riot.RDFFormat;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;
import org.springframework.web.bind.annotation.GetMapping;

import java.io.StringWriter;

@Service
public class RDFMapperImpl implements RDFMapper {
    @Value("${vocabulary-server.base-url}")
    private String baseUrl;

    public static final RDFFormat RDF_XML_SYNTAX = RDFFormat.RDFXML;
    public static final RDFFormat RDF_TURTLE_SYNTAX = RDFFormat.TURTLE_BLOCKS;

    @Override
    public String toRDFXML(LanguageEntity entity) {
        return transform(entity, this::generateLanguageModel, RDF_XML_SYNTAX);
    }

    @Override
    public String toRDFTurtle(LanguageEntity entity) {
        return transform(entity, this::generateLanguageModel, RDF_TURTLE_SYNTAX);
    }

    private <T> String transform(T entity, EntityToModelMappingFunction<T> function, RDFFormat format) {
        StringWriter out = new StringWriter();
        RDFDataMgr.write(out, function.map(entity), format);
        return out.toString();
    }

    private Model generateLanguageModel(LanguageEntity entity) {
        String uri = generateLanguageURIForId(entity.getId());

        Model model = ModelFactory.createDefaultModel();

        Resource resource = model.createResource(uri)
                .addProperty(LANGUAGE.NAME, entity.getName())
                .addProperty(LANGUAGE.ABBREVIATION, entity.getAbbreviation());

        return model;
    }

    private String generateLanguageURIForId(long id) {
        try {
            String[] values = LanguageController.class.getMethod("one", long.class).getAnnotation(GetMapping.class).value();
            assert (values.length == 1);
            String endpoint = values[0].replace("{id}", Long.toString(id));
            return baseUrl + endpoint;
        } catch (NoSuchMethodException e) {
            throw new MappingException(LanguageEntity.class, String.class, e);
        }
    }
}
