package io.goobi.vocabularyserver.exchange;

import lombok.Data;
import org.springframework.hateoas.EntityModel;

import java.util.Set;

@Data
public class VocabularyRecordDTO {
    private Long id;
    private Long parentId;
    private Long vocabularyId;
    private Set<FieldInstanceDTO> fields;
    private Set<EntityModel<VocabularyRecordDTO>> children;
}
