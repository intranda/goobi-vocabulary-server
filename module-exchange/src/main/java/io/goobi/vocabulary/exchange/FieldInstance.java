package io.goobi.vocabulary.exchange;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.Data;
import lombok.EqualsAndHashCode;

import java.util.LinkedList;
import java.util.List;

@Data
@EqualsAndHashCode(callSuper = false)
@JsonIgnoreProperties(ignoreUnknown = true)
public class FieldInstance extends BaseObject {
    private Long id;
    private Long recordId;
    private Long definitionId;
    private List<FieldValue> values = new LinkedList<>();
}
