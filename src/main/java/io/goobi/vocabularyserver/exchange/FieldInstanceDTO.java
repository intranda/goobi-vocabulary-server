package io.goobi.vocabularyserver.exchange;

import lombok.Data;

import java.util.HashSet;
import java.util.Set;

@Data
public class FieldInstanceDTO {
    private long id;
    private long recordId;
    private long definitionId;
    private Set<FieldValueDTO> values = new HashSet<>();
}
