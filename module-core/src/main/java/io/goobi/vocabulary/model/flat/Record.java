package io.goobi.vocabulary.model.flat;

import lombok.Data;

import java.util.List;

@Data
public class Record {
    private long id;
    private List<Record> children;
    private List<Field> fields;
}
