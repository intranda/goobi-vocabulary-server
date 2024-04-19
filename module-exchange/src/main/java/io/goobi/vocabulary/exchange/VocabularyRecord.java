package io.goobi.vocabulary.exchange;

import com.fasterxml.jackson.dataformat.xml.annotation.JacksonXmlRootElement;
import lombok.Data;
import org.springframework.hateoas.EntityModel;

import java.util.Set;

@Data
@JacksonXmlRootElement(localName = "record")
public class VocabularyRecord {
    private Long id;
    private Long parentId;
    private Long vocabularyId;
    private Set<FieldInstance> fields;
    private Set<EntityModel<VocabularyRecord>> children;
}
