package io.goobi.vocabularyserver.exchange;

import lombok.Data;

@Data
public class FieldDefinitionDTO {
    private long id;
    private long schemaId;
    private String name;
    private long typeId;
    private boolean required;
    private boolean unique;
    private boolean mainEntry;
    private boolean titleField;
    private boolean multiValued;
}
