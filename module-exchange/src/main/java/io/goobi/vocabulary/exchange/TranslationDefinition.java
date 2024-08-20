package io.goobi.vocabulary.exchange;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.Data;
import lombok.EqualsAndHashCode;

@Data
@EqualsAndHashCode(callSuper = false)
@JsonIgnoreProperties(ignoreUnknown = true)
public class TranslationDefinition extends BaseObject{
    private Long id;
    private Long definitionId;
    private String language;
    private Boolean fallback;
    private Boolean required;
}
