package io.goobi.vocabularyserver.service.exchange;

import io.goobi.vocabularyserver.exchange.FieldTypeDTO;
import io.goobi.vocabularyserver.model.FieldType;
import io.goobi.vocabularyserver.model.SelectableValue;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;

import java.util.Set;
import java.util.stream.Collectors;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

@SpringBootTest
class FieldTypeMapperTest {
    private static final Long FIELD_TYPE_ID = 624L;
    private static final String FIELD_TYPE_NAME = "Text";
    private static final String FIELD_TYPE_VALIDATION = "\\w+";
    private static final Set<String> FIELD_TYPE_SELECTABLE_VALUES = Set.of("Value1", "2ndValue", "value numero three");

    @Autowired
    private DTOMapper mapper;

    private FieldType fieldType;
    private FieldTypeDTO fieldTypeDTO;

    @BeforeEach
    void setUp() {
        fieldType = new FieldType(FIELD_TYPE_NAME);
        fieldType.setId(FIELD_TYPE_ID);
        fieldType.setValidation(FIELD_TYPE_VALIDATION);
        fieldType.setSelectableValues(FIELD_TYPE_SELECTABLE_VALUES.stream().map(SelectableValue::new).collect(Collectors.toSet()));
        fieldTypeDTO = new FieldTypeDTO();
        fieldTypeDTO.setId(FIELD_TYPE_ID);
        fieldTypeDTO.setName(FIELD_TYPE_NAME);
        fieldTypeDTO.setValidation(FIELD_TYPE_VALIDATION);
        fieldTypeDTO.setSelectableValues(FIELD_TYPE_SELECTABLE_VALUES);
    }

    @Test
    void validId_toDTO() {
        FieldTypeDTO result = mapper.toDTO(fieldType);

        assertEquals(FIELD_TYPE_ID, result.getId());
    }

    @Test
    void validName_toDTO() {
        FieldTypeDTO result = mapper.toDTO(fieldType);

        assertEquals(FIELD_TYPE_NAME, result.getName());
    }

    @Test
    void existingValidation_toDTO() {
        FieldTypeDTO result = mapper.toDTO(fieldType);

        assertEquals(FIELD_TYPE_VALIDATION, result.getValidation());
    }

    @Test
    void existingSelectableValues_toDTO() {
        FieldTypeDTO result = mapper.toDTO(fieldType);

        assertEquals(FIELD_TYPE_SELECTABLE_VALUES, result.getSelectableValues());
    }

    @Test
    void emptyValidation_toDTO() {
        fieldType.setValidation(null);

        FieldTypeDTO result = mapper.toDTO(fieldType);

        assertNull(result.getValidation());
    }

    @Test
    void emptySelectableValues_toDTO() {
        fieldType.setSelectableValues(null);

        FieldTypeDTO result = mapper.toDTO(fieldType);

        assertNull(result.getSelectableValues());
    }

    @Test
    void validId_fromDTO() {
        FieldType result = mapper.toEntity(fieldTypeDTO);

        assertEquals(FIELD_TYPE_ID, result.getId());
    }

    @Test
    void validName_fromDTO() {
        FieldType result = mapper.toEntity(fieldTypeDTO);

        assertEquals(FIELD_TYPE_NAME, result.getName());
    }

    @Test
    void existingValidation_fromDTO() {
        FieldType result = mapper.toEntity(fieldTypeDTO);

        assertEquals(FIELD_TYPE_VALIDATION, result.getValidation());
    }

    @Test
    void existingSelectableValues_fromDTO() {
        FieldType result = mapper.toEntity(fieldTypeDTO);

        assertEquals(FIELD_TYPE_SELECTABLE_VALUES, result.getSelectableValues().stream().map(SelectableValue::getValue).collect(Collectors.toSet()));
    }

    @Test
    void emptyValidation_fromDTO() {
        fieldTypeDTO.setValidation(null);

        FieldType result = mapper.toEntity(fieldTypeDTO);

        assertNull(result.getValidation());
    }

    @Test
    void emptySelectableValues_fromDTO() {
        fieldTypeDTO.setSelectableValues(null);

        FieldType result = mapper.toEntity(fieldTypeDTO);

        assertTrue(result.getSelectableValues().isEmpty());
    }
}
