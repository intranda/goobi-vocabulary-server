package io.goobi.vocabulary.service.rdf.vocabulary;

import org.apache.jena.rdf.model.Model;
import org.apache.jena.rdf.model.ModelFactory;
import org.apache.jena.rdf.model.Property;
import org.apache.jena.rdf.model.Resource;

public class LANGUAGE {
    public static final String URI = "http://www.w3.org/2024/language-rdf/1.0#";
    private static final Model m = ModelFactory.createDefaultModel();
    // TODO: Example for reference
    public static final Resource TELTYPES;
    public static final Property NAME;
    public static final Property ABBREVIATION;

    static {
        TELTYPES = m.createResource(URI + "TELTYPES");
        NAME = m.createProperty(URI, "Name");
        ABBREVIATION = m.createProperty(URI, "Abbreviation");
    }
}
