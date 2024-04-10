package io.goobi.vocabularyserver.service.exchange;

import io.goobi.vocabularyserver.exchange.FieldDefinition;
import io.goobi.vocabularyserver.model.FieldDefinitionEntity;
import io.goobi.vocabularyserver.model.FieldTypeEntity;
import io.goobi.vocabularyserver.model.VocabularySchemaEntity;
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

    private VocabularySchemaEntity schema;
    private FieldTypeEntity fieldType;
    private FieldDefinitionEntity fieldDefinition;
    private FieldDefinition fieldDefinitionDTO;

    @BeforeEach
    void setUp() {
        schema = new VocabularySchemaEntity();
        schema.setId(SCHEMA_ID);

        fieldType = new FieldTypeEntity();
        fieldType.setId(FIELD_TYPE_ID);

        FieldDefinitionEntity child = new FieldDefinitionEntity();
        child.setSchema(schema);
        child.setName("Child");
        child.setType(fieldType);
        schema.getDefinitions().add(child);

        fieldDefinition = new FieldDefinitionEntity();
        fieldDefinition.setId(FIELD_DEFINITION_ID);
        fieldDefinition.setSchema(schema);
        fieldDefinition.setName(FIELD_DEFINITION_NAME);
        fieldDefinition.setType(fieldType);
        fieldDefinitionDTO = new FieldDefinition();
        fieldDefinitionDTO.setId(FIELD_DEFINITION_ID);
        fieldDefinitionDTO.setSchemaId(SCHEMA_ID);
        fieldDefinitionDTO.setName(FIELD_DEFINITION_NAME);
        fieldDefinitionDTO.setTypeId(FIELD_TYPE_ID);

        when(fieldTypeRepository.findById(FIELD_TYPE_ID)).thenReturn(Optional.of(fieldType));
        when(vocabularySchemaRepository.findById(SCHEMA_ID)).thenReturn(Optional.of(schema));
    }

    @Test
    void validId_toDTO() {
        FieldDefinition result = mapper.toDTO(fieldDefinition);

        assertEquals(FIELD_DEFINITION_ID, result.getId());
    }

    @Test
    void validSchema_toDTO() {
        FieldDefinition result = mapper.toDTO(fieldDefinition);

        assertEquals(SCHEMA_ID, result.getSchemaId());
    }

    @Test
    void validName_toDTO() {
        FieldDefinition result = mapper.toDTO(fieldDefinition);

        assertEquals(FIELD_DEFINITION_NAME, result.getName());
    }

    @Test
    void validType_toDTO() {
        FieldDefinition result = mapper.toDTO(fieldDefinition);

        assertEquals(FIELD_TYPE_ID, result.getTypeId());
    }

    @Test
    void notRequired_toDTO() {
        fieldDefinition.setRequired(false);

        FieldDefinition result = mapper.toDTO(fieldDefinition);

        assertFalse(result.getRequired());
    }

    @Test
    void notUnique_toDTO() {
        fieldDefinition.setUnique(false);

        FieldDefinition result = mapper.toDTO(fieldDefinition);

        assertFalse(result.getUnique());
    }

    @Test
    void notMainEntry_toDTO() {
        fieldDefinition.setMainEntry(false);

        FieldDefinition result = mapper.toDTO(fieldDefinition);

        assertFalse(result.getMainEntry());
    }

    @Test
    void notTitleField_toDTO() {
        fieldDefinition.setTitleField(false);

        FieldDefinition result = mapper.toDTO(fieldDefinition);

        assertFalse(result.getTitleField());
    }

    @Test
    void notMultiValued_toDTO() {
        fieldDefinition.setMultiValued(false);

        FieldDefinition result = mapper.toDTO(fieldDefinition);

        assertFalse(result.getMultiValued());
    }

    @Test
    void isRequired_toDTO() {
        fieldDefinition.setRequired(true);

        FieldDefinition result = mapper.toDTO(fieldDefinition);

        assertTrue(result.getRequired());
    }

    @Test
    void isUnique_toDTO() {
        fieldDefinition.setUnique(true);

        FieldDefinition result = mapper.toDTO(fieldDefinition);

        assertTrue(result.getUnique());
    }

    @Test
    void isMainEntry_toDTO() {
        fieldDefinition.setMainEntry(true);

        FieldDefinition result = mapper.toDTO(fieldDefinition);

        assertTrue(result.getMainEntry());
    }

    @Test
    void isTitleField_toDTO() {
        fieldDefinition.setTitleField(true);

        FieldDefinition result = mapper.toDTO(fieldDefinition);

        assertTrue(result.getTitleField());
    }

    @Test
    void isMultiValued_toDTO() {
        fieldDefinition.setMultiValued(true);

        FieldDefinition result = mapper.toDTO(fieldDefinition);

        assertTrue(result.getMultiValued());
    }

    @Test
    void validId_fromDTO() {
        FieldDefinitionEntity result = mapper.toEntity(fieldDefinitionDTO);

        assertEquals(FIELD_DEFINITION_ID, result.getId());
    }

    @Test
    void validSchema_fromDTO() {
        FieldDefinitionEntity result = mapper.toEntity(fieldDefinitionDTO);

        assertEquals(schema, result.getSchema());
    }

    @Test
    void validName_fromDTO() {
        FieldDefinitionEntity result = mapper.toEntity(fieldDefinitionDTO);

        assertEquals(FIELD_DEFINITION_NAME, result.getName());
    }

    @Test
    void validType_fromDTO() {
        FieldDefinitionEntity result = mapper.toEntity(fieldDefinitionDTO);

        assertEquals(fieldType, result.getType());
    }

    @Test
    void notRequired_fromDTO() {
        fieldDefinitionDTO.setRequired(false);

        FieldDefinitionEntity result = mapper.toEntity(fieldDefinitionDTO);

        assertFalse(result.isRequired());
    }

    @Test
    void notUnique_fromDTO() {
        fieldDefinitionDTO.setUnique(false);

        FieldDefinitionEntity result = mapper.toEntity(fieldDefinitionDTO);

        assertFalse(result.isUnique());
    }

    @Test
    void notMainEntry_fromDTO() {
        fieldDefinitionDTO.setMainEntry(false);

        FieldDefinitionEntity result = mapper.toEntity(fieldDefinitionDTO);

        assertNull(result.getMainEntry());
    }

    @Test
    void notTitleField_fromDTO() {
        fieldDefinitionDTO.setTitleField(false);

        FieldDefinitionEntity result = mapper.toEntity(fieldDefinitionDTO);

        assertFalse(result.isTitleField());
    }

    @Test
    void notMultiValued_fromDTO() {
        fieldDefinitionDTO.setMultiValued(false);

        FieldDefinitionEntity result = mapper.toEntity(fieldDefinitionDTO);

        assertFalse(result.isMultiValued());
    }

    @Test
    void isRequired_fromDTO() {
        fieldDefinitionDTO.setRequired(true);

        FieldDefinitionEntity result = mapper.toEntity(fieldDefinitionDTO);

        assertTrue(result.isRequired());
    }

    @Test
    void isUnique_fromDTO() {
        fieldDefinitionDTO.setUnique(true);

        FieldDefinitionEntity result = mapper.toEntity(fieldDefinitionDTO);

        assertTrue(result.isUnique());
    }

    @Test
    void isMainEntry_fromDTO() {
        fieldDefinitionDTO.setMainEntry(true);

        FieldDefinitionEntity result = mapper.toEntity(fieldDefinitionDTO);

        assertTrue(result.getMainEntry());
    }

    @Test
    void isTitleField_fromDTO() {
        fieldDefinitionDTO.setTitleField(true);

        FieldDefinitionEntity result = mapper.toEntity(fieldDefinitionDTO);

        assertTrue(result.isTitleField());
    }

    @Test
    void isMultiValued_fromDTO() {
        fieldDefinitionDTO.setMultiValued(true);

        FieldDefinitionEntity result = mapper.toEntity(fieldDefinitionDTO);

        assertTrue(result.isMultiValued());
    }
}
