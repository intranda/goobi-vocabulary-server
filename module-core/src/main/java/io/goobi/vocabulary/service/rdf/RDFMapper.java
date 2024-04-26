package io.goobi.vocabulary.service.rdf;

import io.goobi.vocabulary.model.jpa.LanguageEntity;

public interface RDFMapper {
    String toRDFXML(LanguageEntity entity);
    String toRDFTurtle(LanguageEntity entity);
}
