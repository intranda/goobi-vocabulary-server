package io.goobi.vocabularyserver.exchange;

import lombok.Data;

import java.util.Set;

@Data
public class VocabularyRecordDTO {
    private long id;
    private long vocabularyId;
    private Set<FieldInstanceDTO> fields;
}
