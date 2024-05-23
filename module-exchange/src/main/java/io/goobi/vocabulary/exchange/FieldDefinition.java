package io.goobi.vocabulary.exchange;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.Data;
import lombok.EqualsAndHashCode;

import java.util.Set;

@Data
@EqualsAndHashCode(callSuper = false)
@JsonIgnoreProperties(ignoreUnknown = true)
public class FieldDefinition extends BaseObject {
    private Long id;
    private Long schemaId;
    private String name;
    private Long typeId;
    private Long referenceVocabularyId;
    private Boolean required;
    private Boolean unique;
    private Boolean mainEntry;
    private Boolean titleField;
    private Boolean multiValued;
    private Set<TranslationDefinition> translationDefinitions;
}
