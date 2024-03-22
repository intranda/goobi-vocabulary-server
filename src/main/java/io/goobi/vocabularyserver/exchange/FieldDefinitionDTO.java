package io.goobi.vocabularyserver.exchange;

import lombok.Data;

@Data
public class FieldDefinitionDTO {
    private long id;
    private String language;
    private String name;
    private long typeId;
    private boolean required;
    private boolean unique;
    private boolean mainEntry;
    private boolean titleField;
}
