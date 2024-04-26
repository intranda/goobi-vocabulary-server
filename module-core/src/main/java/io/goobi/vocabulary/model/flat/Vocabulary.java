package io.goobi.vocabulary.model.flat;

import lombok.Data;

import java.util.List;

@Data
public class Vocabulary {
    private long id;
    private String name;
    private String description;
    private Schema schema;
    private List<Record> entries;
}
