package io.goobi.vocabulary.exception;

import io.goobi.vocabulary.model.jpa.VocabularyEntity;

import java.util.Set;
import java.util.stream.Collectors;

public class DeletionOfReferencedVocabularyException extends IllegalStateException {
    public DeletionOfReferencedVocabularyException(VocabularyEntity vocabulary, Set<VocabularyEntity> referencedVocabularies) {
        super("Cannot remove vocabulary [" + vocabulary.getId() + "], it is referenced in the following vocabularies:\n\t" +
                referencedVocabularies.stream()
                        .map(v -> v.getName() + " [" + v.getId() + "]")
                        .collect(Collectors.joining("\n\t")));
    }
}
