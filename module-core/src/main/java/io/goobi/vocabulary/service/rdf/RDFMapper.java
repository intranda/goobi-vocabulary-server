package io.goobi.vocabulary.service.rdf;

import io.goobi.vocabulary.model.jpa.VocabularyEntity;

public interface RDFMapper {
    boolean isRDFCompatible(VocabularyEntity entity);

    String toRDFXML(VocabularyEntity entity);

    String toRDFTurtle(VocabularyEntity entity);
}
