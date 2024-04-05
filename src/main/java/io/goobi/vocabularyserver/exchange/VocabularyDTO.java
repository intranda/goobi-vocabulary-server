package io.goobi.vocabularyserver.exchange;

import lombok.Data;

@Data
public class VocabularyDTO {
    private Long id;
    private Long schemaId;
    private String name;
    private String description;
}
