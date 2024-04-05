package io.goobi.vocabularyserver.exchange;

import lombok.Data;

@Data
public class FieldDefinitionDTO {
    private Long id;
    private Long schemaId;
    private String name;
    private Long typeId;
    private Boolean required;
    private Boolean unique;
    private Boolean mainEntry;
    private Boolean titleField;
    private Boolean multiValued;
}
