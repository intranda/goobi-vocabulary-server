package io.goobi.vocabularyserver.exchange;

import lombok.Data;

@Data
public class VocabularyDTO {
    private long id;
    private long schemaId;
    private String name;
    private String description;
}
