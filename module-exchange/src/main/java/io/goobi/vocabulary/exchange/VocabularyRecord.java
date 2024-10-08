package io.goobi.vocabulary.exchange;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.dataformat.xml.annotation.JacksonXmlRootElement;
import lombok.Data;
import lombok.EqualsAndHashCode;

import java.util.Set;

@Data
@EqualsAndHashCode(callSuper = false)
@JsonIgnoreProperties(ignoreUnknown = true)
@JacksonXmlRootElement(localName = "record")
public class VocabularyRecord extends BaseObject {
    private Long id;
    private Long parentId;
    private Long vocabularyId;
    private Boolean metadata;
    private Set<FieldInstance> fields;
    private Set<Long> children;
}
