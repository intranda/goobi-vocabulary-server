package io.goobi.vocabulary.exchange;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.Data;
import lombok.EqualsAndHashCode;

import java.util.List;

@Data
@EqualsAndHashCode(callSuper = false)
@JsonIgnoreProperties(ignoreUnknown = true)
public class VocabularySchema extends BaseObject {
    private Long id;
    private List<FieldDefinition> definitions;
    private Boolean hierarchicalRecords;
    private Boolean singleRootElement;
}
