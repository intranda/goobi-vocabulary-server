package io.goobi.vocabularyserver.service.rdf;

import org.apache.jena.rdf.model.Model;

public interface EntityToModelMappingFunction<T> {
    Model map(T entity);
}
