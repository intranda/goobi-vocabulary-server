package io.goobi.vocabulary.model.flat;

import lombok.Data;

import java.util.List;

@Data
public class Field {
    private long id;
    private long definitionId;
    private List<Value> values;
}
