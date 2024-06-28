package io.goobi.vocabulary.exception;

import io.goobi.vocabulary.model.jpa.VocabularyEntity;
import io.goobi.vocabulary.model.jpa.VocabularyRecordEntity;

import java.util.List;
import java.util.stream.Collectors;

public class DeletionOfReferencedRecordException extends IllegalStateException {
    public DeletionOfReferencedRecordException(VocabularyRecordEntity rec, List<VocabularyRecordEntity> referencingRecords) {
        super("Cannot delete record [" + rec.getId() + "], it is referenced by the following records:\n\t" +
                referencingRecords.stream()
                        .map(Object::toString)
                        .collect(Collectors.joining("\n\t")));
    }

    public DeletionOfReferencedRecordException(VocabularyEntity vocabulary, List<DeletionOfReferencedRecordException> causes) {
        super("Cannot remove all records of vocabulary [" + vocabulary.getId() + "], reason:\n" + causes.stream().map(Throwable::getMessage).collect(Collectors.joining((CharSequence) "\n")));
    }
}
