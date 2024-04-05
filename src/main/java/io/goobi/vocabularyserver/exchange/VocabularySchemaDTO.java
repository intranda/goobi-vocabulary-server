package io.goobi.vocabularyserver.exchange;

import lombok.Data;

import java.util.List;

@Data
public class VocabularySchemaDTO {
    private Long id;
    private List<FieldDefinitionDTO> definitions;
    private Boolean hierarchicalRecords;
}
