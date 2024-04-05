package io.goobi.vocabularyserver.exchange;

import lombok.Data;

import java.util.HashMap;
import java.util.Map;

@Data
public class FieldValueDTO {
    private Long id;
    private Long fieldId;
    private Map<String, String> translations = new HashMap<>();
}
