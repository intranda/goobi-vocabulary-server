package io.goobi.vocabularyserver.exchange;

import lombok.Data;

import java.util.HashSet;
import java.util.Set;

@Data
public class FieldInstanceDTO {
    private Long id;
    private Long recordId;
    private Long definitionId;
    private Set<FieldValueDTO> values = new HashSet<>();
}
