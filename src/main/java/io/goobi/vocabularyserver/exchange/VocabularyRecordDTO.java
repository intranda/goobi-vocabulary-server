package io.goobi.vocabularyserver.exchange;

import lombok.Data;

import java.util.Set;

@Data
public class VocabularyRecordDTO {
    private long id;
    private Long parentId;
    private long vocabularyId;
    private Set<FieldInstanceDTO> fields;
    private Set<VocabularyRecordDTO> children;
}
