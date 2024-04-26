package io.goobi.vocabulary.service.rdf;

import io.goobi.vocabulary.api.LanguageController;
import io.goobi.vocabulary.exception.MappingException;
import io.goobi.vocabulary.model.jpa.LanguageEntity;
import io.goobi.vocabulary.service.rdf.vocabulary.LANGUAGE;
import org.apache.jena.rdf.model.Model;
import org.apache.jena.rdf.model.ModelFactory;
import org.apache.jena.rdf.model.Resource;
import org.apache.jena.riot.RDFDataMgr;
import org.apache.jena.riot.RDFFormat;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;

import java.io.StringWriter;
import java.lang.reflect.Method;

@Service
public class RDFMapperImpl implements RDFMapper {
    @Value("${vocabulary-server.base-url}")
    private String host;

    @Value("${server.port}")
    private int port;

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
            String classRoute = extractClassEndpoint(LanguageController.class);
            String methodRoute = extractMethodEndpoint(LanguageController.class.getMethod("one", long.class));
            String endpoint = classRoute + methodRoute.replace("{id}", Long.toString(id));
            return host + ':' + port + endpoint;
        } catch (NoSuchMethodException e) {
            throw new MappingException(LanguageEntity.class, String.class, e);
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
}
