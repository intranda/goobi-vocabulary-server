package io.goobi.vocabularyserver.exchange;

import lombok.Data;

import java.util.HashSet;
import java.util.Set;

@Data
public class FieldInstance {
    private Long id;
    private Long recordId;
    private Long definitionId;
    private Set<FieldValue> values = new HashSet<>();
}
