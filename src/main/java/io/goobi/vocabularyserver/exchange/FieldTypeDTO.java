package io.goobi.vocabularyserver.exchange;

import lombok.Data;

import java.util.Set;

@Data
public class FieldTypeDTO {
    private long id;
    private String name;
    private String validation;
    private Set<String> selectableValues;
}
