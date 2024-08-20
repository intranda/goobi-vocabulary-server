package io.goobi.vocabulary.model.flat;

import lombok.Data;

import java.util.List;

@Data
public class Schema {
    private long id;
    private boolean hierarchicalRecordsEnabled;
    private boolean singleRootElement;
    private List<Definition> definitions;
}
