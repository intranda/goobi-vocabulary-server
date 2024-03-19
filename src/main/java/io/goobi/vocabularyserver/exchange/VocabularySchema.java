package io.goobi.vocabularyserver.exchange;

import lombok.Data;

import java.util.List;

@Data
public class VocabularySchema {
    private long id;
    private List<FieldDefinition> definitions;
}
