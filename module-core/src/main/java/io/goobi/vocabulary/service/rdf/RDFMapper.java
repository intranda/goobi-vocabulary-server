package io.goobi.vocabulary.service.rdf;

import io.goobi.vocabulary.model.jpa.LanguageEntity;
import io.goobi.vocabulary.model.jpa.VocabularyEntity;

public interface RDFMapper {
    String toRDFXML(LanguageEntity entity);
    String toRDFXML(VocabularyEntity entity);
    String toRDFTurtle(LanguageEntity entity);
}
