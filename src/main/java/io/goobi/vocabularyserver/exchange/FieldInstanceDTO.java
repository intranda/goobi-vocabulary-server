package io.goobi.vocabularyserver.exchange;

import lombok.Data;

@Data
public class FieldInstanceDTO {
    private long id;
    private long recordId;
    private long definitionId;
    private String language;
    private String value;
}
