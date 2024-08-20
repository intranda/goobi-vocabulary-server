package io.goobi.vocabulary.service.exchange;

import io.goobi.vocabulary.exchange.FieldType;
import io.goobi.vocabulary.model.jpa.FieldTypeEntity;
import io.goobi.vocabulary.model.jpa.SelectableValueEntity;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.function.Executable;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;

import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
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

    private FieldTypeEntity fieldType;
    private FieldType fieldTypeDTO;

    @BeforeEach
    void setUp() {
        fieldType = new FieldTypeEntity();
        fieldType.setId(FIELD_TYPE_ID);
        fieldType.setName(FIELD_TYPE_NAME);
        fieldType.setValidation(FIELD_TYPE_VALIDATION);
        fieldType.setSelectableValues(FIELD_TYPE_SELECTABLE_VALUES.stream().map(s -> {
            SelectableValueEntity sv = new SelectableValueEntity();
            sv.setValue(s);
            return sv;
        }).collect(Collectors.toList()));
        fieldTypeDTO = new FieldType();
        fieldTypeDTO.setId(FIELD_TYPE_ID);
        fieldTypeDTO.setName(FIELD_TYPE_NAME);
        fieldTypeDTO.setValidation(FIELD_TYPE_VALIDATION);
        fieldTypeDTO.setSelectableValues(FIELD_TYPE_SELECTABLE_VALUES);
    }

    @Test
    void validId_toDTO() {
        FieldType result = mapper.toDTO(fieldType);

        assertEquals(FIELD_TYPE_ID, result.getId());
    }

    @Test
    void validName_toDTO() {
        FieldType result = mapper.toDTO(fieldType);

        assertEquals(FIELD_TYPE_NAME, result.getName());
    }

    @Test
    void existingValidation_toDTO() {
        FieldType result = mapper.toDTO(fieldType);

        assertEquals(FIELD_TYPE_VALIDATION, result.getValidation());
    }

    @Test
    void existingSelectableValues_toDTO() {
        FieldType result = mapper.toDTO(fieldType);

        assertEquals(FIELD_TYPE_SELECTABLE_VALUES, result.getSelectableValues());
    }

    @Test
    void emptyValidation_toDTO() {
        fieldType.setValidation(null);

        FieldType result = mapper.toDTO(fieldType);

        assertNull(result.getValidation());
    }

    @Test
    void emptySelectableValues_toDTO() {
        fieldType.setSelectableValues(null);

        FieldType result = mapper.toDTO(fieldType);

        assertNull(result.getSelectableValues());
    }

    @Test
    void largeFalse_toDTO() {
        fieldType.setLarge(false);

        FieldType result = mapper.toDTO(fieldType);

        assertEquals(Boolean.FALSE, result.getLarge());
    }

    @Test
    void largeTrue_toDTO() {
        fieldType.setLarge(true);

        FieldType result = mapper.toDTO(fieldType);

        assertEquals(Boolean.TRUE, result.getLarge());
    }

    @Test
    void validId_fromDTO() {
        FieldTypeEntity result = mapper.toEntity(fieldTypeDTO);

        assertEquals(FIELD_TYPE_ID, result.getId());
    }

    @Test
    void validName_fromDTO() {
        FieldTypeEntity result = mapper.toEntity(fieldTypeDTO);

        assertEquals(FIELD_TYPE_NAME, result.getName());
    }

    @Test
    void existingValidation_fromDTO() {
        FieldTypeEntity result = mapper.toEntity(fieldTypeDTO);

        assertEquals(FIELD_TYPE_VALIDATION, result.getValidation());
    }

    @Test
    void existingSelectableValues_fromDTO() {
        FieldTypeEntity result = mapper.toEntity(fieldTypeDTO);

        assertAll(
                () -> assertEquals(FIELD_TYPE_SELECTABLE_VALUES, result.getSelectableValues().stream().map(SelectableValueEntity::getValue).collect(Collectors.toSet())),
                () -> assertCorrectFieldTypeReference(result, result.getSelectableValues())
        );
    }

    private void assertCorrectFieldTypeReference(FieldTypeEntity result, List<SelectableValueEntity> selectableValues) {
        assertAll(
                selectableValues.stream()
                        .map(v -> (Executable) () -> assertEquals(result, v.getFieldType()))
                        .collect(Collectors.toSet())
        );
    }

    @Test
    void emptyValidation_fromDTO() {
        fieldTypeDTO.setValidation(null);

        FieldTypeEntity result = mapper.toEntity(fieldTypeDTO);

        assertNull(result.getValidation());
    }

    @Test
    void emptySelectableValues_fromDTO() {
        fieldTypeDTO.setSelectableValues(null);

        FieldTypeEntity result = mapper.toEntity(fieldTypeDTO);

        assertTrue(result.getSelectableValues().isEmpty());
    }

    @Test
    void largeFalse_fromDTO() {
        fieldTypeDTO.setLarge(false);

        FieldTypeEntity result = mapper.toEntity(fieldTypeDTO);

        assertFalse(result.isLarge());
    }

    @Test
    void largeTrue_fromDTO() {
        fieldTypeDTO.setLarge(true);

        FieldTypeEntity result = mapper.toEntity(fieldTypeDTO);

        assertTrue(result.isLarge());
    }
}
