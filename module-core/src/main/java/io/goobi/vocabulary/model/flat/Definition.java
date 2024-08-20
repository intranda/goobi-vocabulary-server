package io.goobi.vocabulary.model.flat;

import lombok.Data;

import java.util.List;

@Data
public class Definition {
    private long id;
    private String label;
    private Type type;
    private List<LanguageSpecification> languageSpecifications;
    private boolean required;
    private boolean unique;
    private boolean mainEntry;
    private boolean titleField;
    private boolean multiValued;
}
