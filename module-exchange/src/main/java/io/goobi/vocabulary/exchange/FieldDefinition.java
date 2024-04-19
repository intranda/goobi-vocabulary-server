package io.goobi.vocabulary.exchange;

import lombok.Data;

import java.util.Set;

@Data
public class FieldDefinition {
    private Long id;
    private Long schemaId;
    private String name;
    private Long typeId;
    private Boolean required;
    private Boolean unique;
    private Boolean mainEntry;
    private Boolean titleField;
    private Boolean multiValued;
    private Set<TranslationDefinition> translationDefinitions;
}
