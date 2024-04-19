package io.goobi.vocabulary.service.rdf;

import org.apache.jena.rdf.model.Model;

public interface EntityToModelMappingFunction<T> {
    Model map(T entity);
}
