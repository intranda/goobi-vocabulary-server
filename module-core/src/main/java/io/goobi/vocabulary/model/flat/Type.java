package io.goobi.vocabulary.model.flat;

import lombok.Data;

import java.util.List;

@Data
public class Type {
    private long id;
    private String name;
    private String validation;
    private List<String> selectableValues;
}
