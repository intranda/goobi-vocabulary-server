package io.goobi.vocabularyserver.exchange;

import lombok.Data;

@Data
public class FieldInstance {
    private long id;
    private String language;
    private String value;
}