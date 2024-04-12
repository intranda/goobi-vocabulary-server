package io.goobi.vocabularyserver.exchange;

import lombok.Data;

import java.util.List;

@Data
public class VocabularySchema {
    private Long id;
    private List<FieldDefinition> definitions;
    private Boolean hierarchicalRecords;
}
