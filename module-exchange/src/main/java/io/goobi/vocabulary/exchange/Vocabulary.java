package io.goobi.vocabulary.exchange;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.Data;
import lombok.EqualsAndHashCode;

@Data
@EqualsAndHashCode(callSuper = false)
@JsonIgnoreProperties(ignoreUnknown = true)
public class Vocabulary extends BaseObject {
    private Long id;
    private Long schemaId;
    private Long metadataSchemaId;
    private String name;
    private String description;
}
