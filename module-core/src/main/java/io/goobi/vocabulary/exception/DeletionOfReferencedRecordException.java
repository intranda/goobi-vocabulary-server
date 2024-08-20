package io.goobi.vocabulary.exception;

import io.goobi.vocabulary.model.jpa.VocabularyEntity;
import io.goobi.vocabulary.model.jpa.VocabularyRecordEntity;

import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

public class DeletionOfReferencedRecordException extends VocabularyException {
    public DeletionOfReferencedRecordException(VocabularyRecordEntity rec, Collection<VocabularyRecordEntity> referencingRecords) {
        super(ErrorCode.DeletionOfReferencedVocabularyRecord, null, Map.of("recordId", String.valueOf(rec.getId()), "referencingRecordIds", referencingRecords.stream().map(VocabularyRecordEntity::getId).map(String::valueOf).collect(Collectors.joining(","))),
                params -> "Cannot delete record [" + params.get("recordId") + "], it is referenced by the following records: " + params.get("referencingRecordIds"));
    }

    public DeletionOfReferencedRecordException(VocabularyRecordEntity rec, List<VocabularyException> causes) {
        super(ErrorCode.DeletionOfReferencedVocabularyRecord, causes, Map.of("recordId", String.valueOf(rec.getId())),
                params -> "Cannot delete record [" + params.get("recordId") + "], reason:\n" + causes.stream().map(Throwable::getMessage).collect(Collectors.joining((CharSequence) "\n")));
    }

    public DeletionOfReferencedRecordException(VocabularyEntity vocabulary, List<VocabularyException> causes) {
        super(ErrorCode.DeletionOfReferencedVocabularyRecord, causes, Map.of("vocabularyId", String.valueOf(vocabulary.getId())),
                params -> "Cannot remove all records of vocabulary [" + params.get("vocabularyId") + "], reason:\n" + causes.stream().map(Throwable::getMessage).collect(Collectors.joining((CharSequence) "\n")));
    }
}
