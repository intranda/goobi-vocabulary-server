package io.goobi.vocabulary.exchange;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.Data;
import lombok.EqualsAndHashCode;

import java.util.HashMap;
import java.util.Map;

@Data
@EqualsAndHashCode(callSuper = false)
@JsonIgnoreProperties(ignoreUnknown = true)
public class FieldValue extends BaseObject {
    private Long id;
    private Long fieldId;
    private Map<String, String> translations = new HashMap<>();
}
