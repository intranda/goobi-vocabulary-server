package io.goobi.vocabularyserver.exchange;

import lombok.Data;

@Data
public class Vocabulary {
    private int id;
    private int schemaId;
    private String name;
    private String description;
}
