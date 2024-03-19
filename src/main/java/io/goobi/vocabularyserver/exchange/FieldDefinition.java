package io.goobi.vocabularyserver.exchange;

import lombok.Data;

@Data
public class FieldDefinition {
    private long id;
    private String language;
    private String name;
    private FieldType type;
    private boolean required;
    private boolean unique;
    private boolean mainEntry;
    private boolean titleField;
}
