package io.goobi.vocabularyserver.exchange;

import lombok.Data;

@Data
public class TranslationDefinition {
    private Long id;
    private Long definitionId;
    private String language;
    private Boolean fallback;
    private Boolean required;
}
