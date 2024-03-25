package io.goobi.vocabularyserver.service.exchange;

import io.goobi.vocabularyserver.exchange.FieldDefinitionDTO;
import io.goobi.vocabularyserver.model.FieldDefinition;
import io.goobi.vocabularyserver.model.FieldType;
import io.goobi.vocabularyserver.model.VocabularySchema;
import io.goobi.vocabularyserver.repositories.FieldTypeRepository;
import io.goobi.vocabularyserver.repositories.VocabularySchemaRepository;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.springframework.boot.test.context.SpringBootTest;

import java.util.Optional;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.when;

@SpringBootTest
class FieldDefinitionMapperTest {
    private static final Long SCHEMA_ID = 5396823543L;
    private static final Long FIELD_DEFINITION_ID = 6234824L;
    private static final String FIELD_DEFINITION_NAME = "SOME_NAME";
    private static final Long FIELD_TYPE_ID = 203763L;

    @Mock
    private FieldTypeRepository fieldTypeRepository;
    @Mock
    private VocabularySchemaRepository vocabularySchemaRepository;

    @InjectMocks
    private DTOMapperImpl mapper;

    private VocabularySchema schema;
    private FieldType fieldType;
    private FieldDefinition fieldDefinition;
    private FieldDefinitionDTO fieldDefinitionDTO;

    @BeforeEach
    void setUp() {
        schema = new VocabularySchema();
        schema.setId(SCHEMA_ID);

        fieldType = new FieldType("Anything_not_tested");
        fieldType.setId(FIELD_TYPE_ID);

        FieldDefinition child = new FieldDefinition(schema, "Child", fieldType);
        schema.getDefinitions().add(child);

        fieldDefinition = new FieldDefinition(schema, FIELD_DEFINITION_NAME, fieldType);
        fieldDefinition.setId(FIELD_DEFINITION_ID);
        fieldDefinitionDTO = new FieldDefinitionDTO();
        fieldDefinitionDTO.setId(FIELD_DEFINITION_ID);
        fieldDefinitionDTO.setSchemaId(SCHEMA_ID);
        fieldDefinitionDTO.setName(FIELD_DEFINITION_NAME);
        fieldDefinitionDTO.setTypeId(FIELD_TYPE_ID);

        when(fieldTypeRepository.findById(FIELD_TYPE_ID)).thenReturn(Optional.of(fieldType));
        when(vocabularySchemaRepository.findById(SCHEMA_ID)).thenReturn(Optional.of(schema));
    }

    @Test
    void validId_toDTO() {
        FieldDefinitionDTO result = mapper.toDTO(fieldDefinition);

        assertEquals(FIELD_DEFINITION_ID, result.getId());
    }

    @Test
    void validSchema_toDTO() {
        FieldDefinitionDTO result = mapper.toDTO(fieldDefinition);

        assertEquals(SCHEMA_ID, result.getSchemaId());
    }

    @Test
    void validName_toDTO() {
        FieldDefinitionDTO result = mapper.toDTO(fieldDefinition);

        assertEquals(FIELD_DEFINITION_NAME, result.getName());
    }

    @Test
    void validType_toDTO() {
        FieldDefinitionDTO result = mapper.toDTO(fieldDefinition);

        assertEquals(FIELD_TYPE_ID, result.getTypeId());
    }

    @Test
    void notRequired_toDTO() {
        fieldDefinition.setRequired(false);

        FieldDefinitionDTO result = mapper.toDTO(fieldDefinition);

        assertFalse(result.isRequired());
    }

    @Test
    void notUnique_toDTO() {
        fieldDefinition.setUnique(false);

        FieldDefinitionDTO result = mapper.toDTO(fieldDefinition);

        assertFalse(result.isUnique());
    }

    @Test
    void notMainEntry_toDTO() {
        fieldDefinition.setMainEntry(false);

        FieldDefinitionDTO result = mapper.toDTO(fieldDefinition);

        assertFalse(result.isMainEntry());
    }

    @Test
    void notTitleField_toDTO() {
        fieldDefinition.setTitleField(false);

        FieldDefinitionDTO result = mapper.toDTO(fieldDefinition);

        assertFalse(result.isTitleField());
    }

    @Test
    void isRequired_toDTO() {
        fieldDefinition.setRequired(true);

        FieldDefinitionDTO result = mapper.toDTO(fieldDefinition);

        assertTrue(result.isRequired());
    }

    @Test
    void isUnique_toDTO() {
        fieldDefinition.setUnique(true);

        FieldDefinitionDTO result = mapper.toDTO(fieldDefinition);

        assertTrue(result.isUnique());
    }

    @Test
    void isMainEntry_toDTO() {
        fieldDefinition.setMainEntry(true);

        FieldDefinitionDTO result = mapper.toDTO(fieldDefinition);

        assertTrue(result.isMainEntry());
    }

    @Test
    void isTitleField_toDTO() {
        fieldDefinition.setTitleField(true);

        FieldDefinitionDTO result = mapper.toDTO(fieldDefinition);

        assertTrue(result.isTitleField());
    }

    @Test
    void validId_fromDTO() {
        FieldDefinition result = mapper.toEntity(fieldDefinitionDTO);

        assertEquals(FIELD_DEFINITION_ID, result.getId());
    }

    @Test
    void validSchema_fromDTO() {
        FieldDefinition result = mapper.toEntity(fieldDefinitionDTO);

        assertEquals(schema, result.getSchema());
    }

    @Test
    void validName_fromDTO() {
        FieldDefinition result = mapper.toEntity(fieldDefinitionDTO);

        assertEquals(FIELD_DEFINITION_NAME, result.getName());
    }

    @Test
    void validType_fromDTO() {
        FieldDefinition result = mapper.toEntity(fieldDefinitionDTO);

        assertEquals(fieldType, result.getType());
    }

    @Test
    void notRequired_fromDTO() {
        fieldDefinitionDTO.setRequired(false);

        FieldDefinition result = mapper.toEntity(fieldDefinitionDTO);

        assertFalse(result.getRequired());
    }

    @Test
    void notUnique_fromDTO() {
        fieldDefinitionDTO.setUnique(false);

        FieldDefinition result = mapper.toEntity(fieldDefinitionDTO);

        assertFalse(result.getUnique());
    }

    @Test
    void notMainEntry_fromDTO() {
        fieldDefinitionDTO.setMainEntry(false);

        FieldDefinition result = mapper.toEntity(fieldDefinitionDTO);

        assertNull(result.getMainEntry());
    }

    @Test
    void notTitleField_fromDTO() {
        fieldDefinitionDTO.setTitleField(false);

        FieldDefinition result = mapper.toEntity(fieldDefinitionDTO);

        assertFalse(result.getTitleField());
    }

    @Test
    void isRequired_fromDTO() {
        fieldDefinitionDTO.setRequired(true);

        FieldDefinition result = mapper.toEntity(fieldDefinitionDTO);

        assertTrue(result.getRequired());
    }

    @Test
    void isUnique_fromDTO() {
        fieldDefinitionDTO.setUnique(true);

        FieldDefinition result = mapper.toEntity(fieldDefinitionDTO);

        assertTrue(result.getUnique());
    }

    @Test
    void isMainEntry_fromDTO() {
        fieldDefinitionDTO.setMainEntry(true);

        FieldDefinition result = mapper.toEntity(fieldDefinitionDTO);

        assertTrue(result.getMainEntry());
    }

    @Test
    void isTitleField_fromDTO() {
        fieldDefinitionDTO.setTitleField(true);

        FieldDefinition result = mapper.toEntity(fieldDefinitionDTO);

        assertTrue(result.getTitleField());
    }
}
