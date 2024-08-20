package io.goobi.vocabulary.model.flat;

import lombok.Data;

@Data
public class LanguageSpecification {
    private long id;
    private String language;
    private boolean fallback;
    private boolean required;
}
