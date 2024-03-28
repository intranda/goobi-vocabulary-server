package io.goobi.vocabularyserver.exchange;

import lombok.Data;

import java.util.Map;

@Data
public class FieldValueDTO {
    private long id;
    private long fieldId;
    private Map<String, String> translations;
}
