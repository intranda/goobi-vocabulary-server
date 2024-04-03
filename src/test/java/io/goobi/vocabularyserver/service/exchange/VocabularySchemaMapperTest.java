package io.goobi.vocabularyserver.service.exchange;

import io.goobi.vocabularyserver.exchange.FieldDefinitionDTO;
import io.goobi.vocabularyserver.exchange.VocabularySchemaDTO;
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

import java.util.List;
import java.util.Optional;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.when;

@SpringBootTest
class VocabularySchemaMapperTest {
    private static final Long SCHEMA_ID = 697230634L;
    private static final Long FIELD_TYPE_ID = 2435L;
    private static final Long FIELD_DEFINITION_1_ID = 45234L;
    private static final String FIELD_DEFINITION_1_NAME = "First name";
    private static final Long FIELD_DEFINITION_2_ID = 423455234L;
    private static final String FIELD_DEFINITION_2_NAME = "Last name";

    private VocabularySchema vocabularySchema;
    private VocabularySchemaDTO vocabularySchemaDTO;

    private FieldDefinition fieldDefinition1;
    private FieldDefinitionDTO fieldDefinitionDTO1;

    private FieldDefinition fieldDefinition2;
    private FieldDefinitionDTO fieldDefinitionDTO2;


    @Mock
    private FieldTypeRepository fieldTypeRepository;
    @Mock
    private VocabularySchemaRepository vocabularySchemaRepository;
    @InjectMocks
    private DTOMapperImpl mapper;

    @BeforeEach
    void setUp() {
        FieldType fieldType = new FieldType();
        fieldType.setId(FIELD_TYPE_ID);
        fieldType.setName("Text");

        vocabularySchema = new VocabularySchema();
        vocabularySchema.setId(SCHEMA_ID);

        fieldDefinition1 = new FieldDefinition();
        fieldDefinition1.setId(FIELD_DEFINITION_1_ID);
        fieldDefinition1.setSchema(vocabularySchema);
        fieldDefinition1.setName(FIELD_DEFINITION_1_NAME);
        fieldDefinition1.setType(fieldType);

        fieldDefinition2 = new FieldDefinition();
        fieldDefinition2.setId(FIELD_DEFINITION_2_ID);
        fieldDefinition2.setSchema(vocabularySchema);
        fieldDefinition2.setName(FIELD_DEFINITION_2_NAME);
        fieldDefinition2.setType(fieldType);

        vocabularySchema.setDefinitions(List.of(fieldDefinition1, fieldDefinition2));

        vocabularySchemaDTO = new VocabularySchemaDTO();
        vocabularySchemaDTO.setId(SCHEMA_ID);

        fieldDefinitionDTO1 = new FieldDefinitionDTO();
        fieldDefinitionDTO1.setId(FIELD_DEFINITION_1_ID);
        fieldDefinitionDTO1.setSchemaId(SCHEMA_ID);
        fieldDefinitionDTO1.setTypeId(FIELD_TYPE_ID);
        fieldDefinitionDTO1.setName(FIELD_DEFINITION_1_NAME);

        fieldDefinitionDTO2 = new FieldDefinitionDTO();
        fieldDefinitionDTO2.setId(FIELD_DEFINITION_2_ID);
        fieldDefinitionDTO2.setSchemaId(SCHEMA_ID);
        fieldDefinitionDTO2.setTypeId(FIELD_TYPE_ID);
        fieldDefinitionDTO2.setName(FIELD_DEFINITION_2_NAME);

        vocabularySchemaDTO.setDefinitions(List.of(fieldDefinitionDTO1, fieldDefinitionDTO2));

        when(vocabularySchemaRepository.findById(SCHEMA_ID)).thenReturn(Optional.of(vocabularySchema));
        when(fieldTypeRepository.findById(FIELD_TYPE_ID)).thenReturn(Optional.of(fieldType));
    }

    @Test
    void validId_toDTO() {
        VocabularySchemaDTO result = mapper.toDTO(vocabularySchema);

        assertEquals(SCHEMA_ID, result.getId());
    }

    @Test
    void childDefinitions_toDTO() {
        VocabularySchemaDTO result = mapper.toDTO(vocabularySchema);

        assertAll("Verify child definitions",
                () -> assertEquals(SCHEMA_ID, result.getDefinitions().get(0).getSchemaId()),
                () -> assertEquals(FIELD_DEFINITION_1_ID, result.getDefinitions().get(0).getId()),
                () -> assertEquals(FIELD_DEFINITION_1_NAME, result.getDefinitions().get(0).getName()),
                () -> assertEquals(SCHEMA_ID, result.getDefinitions().get(1).getSchemaId()),
                () -> assertEquals(FIELD_DEFINITION_2_ID, result.getDefinitions().get(1).getId()),
                () -> assertEquals(FIELD_DEFINITION_2_NAME, result.getDefinitions().get(1).getName())
        );
    }

    @Test
    void noHierarchicalRecords_toDTO() {
        vocabularySchema.setHierarchicalRecords(false);

        VocabularySchemaDTO result = mapper.toDTO(vocabularySchema);

        assertFalse(result.getHierarchicalRecords());
    }

    @Test
    void hierarchicalRecords_toDTO() {
        vocabularySchema.setHierarchicalRecords(true);

        VocabularySchemaDTO result = mapper.toDTO(vocabularySchema);

        assertTrue(result.getHierarchicalRecords());
    }

    @Test
    void validId_fromDTO() {
        VocabularySchema result = mapper.toEntity(vocabularySchemaDTO);

        assertEquals(SCHEMA_ID, result.getId());
    }

    @Test
    void childDefinitions_fromDTO() {
        VocabularySchema result = mapper.toEntity(vocabularySchemaDTO);

        assertAll("Verify child definitions",
                () -> assertEquals(SCHEMA_ID, result.getDefinitions().get(0).getSchema().getId()),
                () -> assertEquals(FIELD_DEFINITION_1_ID, result.getDefinitions().get(0).getId()),
                () -> assertEquals(FIELD_DEFINITION_1_NAME, result.getDefinitions().get(0).getName()),
                () -> assertEquals(SCHEMA_ID, result.getDefinitions().get(1).getSchema().getId()),
                () -> assertEquals(FIELD_DEFINITION_2_ID, result.getDefinitions().get(1).getId()),
                () -> assertEquals(FIELD_DEFINITION_2_NAME, result.getDefinitions().get(1).getName())
        );
    }

    @Test
    void noHierarchicalRecords_fromDTO() {
        vocabularySchemaDTO.setHierarchicalRecords(false);

        VocabularySchema result = mapper.toEntity(vocabularySchemaDTO);

        assertFalse(result.getHierarchicalRecords());
    }

    @Test
    void hierarchicalRecords_fromDTO() {
        vocabularySchemaDTO.setHierarchicalRecords(true);

        VocabularySchema result = mapper.toEntity(vocabularySchemaDTO);

        assertTrue(result.getHierarchicalRecords());
    }
}
