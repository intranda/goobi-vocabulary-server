package io.goobi.vocabulary.exception;

import io.goobi.vocabulary.model.jpa.VocabularyEntity;

import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

public class DeletionOfReferencedVocabularyException extends VocabularyException {
    public DeletionOfReferencedVocabularyException(VocabularyEntity vocabulary, Set<VocabularyEntity> referencedVocabularies) {
        super(ErrorCode.DeletionOfReferencedVocabulary, null, Map.of(
                        "vocabularyId", String.valueOf(vocabulary.getId()),
                        "referencingVocabularyIds", referencedVocabularies.stream().map(VocabularyEntity::getId).map(String::valueOf).collect(Collectors.joining(",")),
                        "referencingVocabularyNames", referencedVocabularies.stream().map(VocabularyEntity::getName).collect(Collectors.joining(","))
                ),
                params -> "Cannot remove vocabulary [" + params.get("vocabularyId") + "], it is referenced in the following vocabularies: " + params.get("referencingVocabularyIds"));
    }
}
