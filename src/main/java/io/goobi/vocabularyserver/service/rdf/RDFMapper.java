package io.goobi.vocabularyserver.service.rdf;

import io.goobi.vocabularyserver.model.LanguageEntity;

public interface RDFMapper {
    String toRDFXML(LanguageEntity entity);
    String toRDFTurtle(LanguageEntity entity);
}
