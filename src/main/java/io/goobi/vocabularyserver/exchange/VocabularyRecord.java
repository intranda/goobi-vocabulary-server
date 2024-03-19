package io.goobi.vocabularyserver.exchange;

import lombok.Data;

import java.util.Set;

@Data
public class VocabularyRecord {
    private long id;
    private Set<FieldInstance> fields;
}
