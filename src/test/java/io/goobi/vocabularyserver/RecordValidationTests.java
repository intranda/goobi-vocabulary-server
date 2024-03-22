package io.goobi.vocabularyserver;

import io.goobi.vocabularyserver.exchange.FieldDefinitionDTO;
import io.goobi.vocabularyserver.exchange.FieldInstanceDTO;
import io.goobi.vocabularyserver.exchange.FieldTypeDTO;
import io.goobi.vocabularyserver.exchange.VocabularyDTO;
import io.goobi.vocabularyserver.exchange.VocabularyRecordDTO;
import io.goobi.vocabularyserver.exchange.VocabularySchemaDTO;
import io.goobi.vocabularyserver.repositories.FieldTypeRepository;
import io.goobi.vocabularyserver.repositories.VocabularyRepository;
import io.goobi.vocabularyserver.repositories.VocabularySchemaRepository;
import io.goobi.vocabularyserver.service.manager.RecordManager;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.springframework.boot.test.context.SpringBootTest;

import java.util.List;
import java.util.Set;

//@DataJpaTest
//This line makes changes to database persist after test
//@Transactional(propagation = Propagation.NOT_SUPPORTED)
//@AutoConfigureTestDatabase(replace = AutoConfigureTestDatabase.Replace.NONE)
@SpringBootTest
public class RecordValidationTests {
    @Mock
    private FieldTypeRepository fieldTypeRepository;
    @Mock
    private VocabularySchemaRepository vocabularySchemaRepository;
    @Mock
    private VocabularyRepository vocabularyRepository;
    @InjectMocks
    private RecordManager recordManager;


    private VocabularySchemaDTO schema;
    private FieldTypeDTO ftText;
    private FieldDefinitionDTO fdName;
    private VocabularyDTO vocabularyDTO;


    @BeforeEach
    public void setUp() {
        ftText = new FieldTypeDTO();
        ftText.setId(1);
        ftText.setName("Text");
        io.goobi.vocabularyserver.model.FieldType ftRawText = new io.goobi.vocabularyserver.model.FieldType(ftText.getName());
        ftRawText.setId(ftText.getId());
//        when(fieldTypeRepository.findById(ftText.getId())).thenReturn(Optional.of(ftRawText));

        schema = new VocabularySchemaDTO();
        fdName = new FieldDefinitionDTO();
        fdName.setName("Name");
        fdName.setMainEntry(true);
        fdName.setTitleField(true);
        fdName.setUnique(true);
        fdName.setRequired(true);
        fdName.setTypeId(ftText.getId());
        schema.setDefinitions(List.of(fdName));
//        schema = vocabularySchemaManager.create(schema);
//        when(vocabularySchemaRepository.findById(schema.getId())).thenReturn(Optional.of(vocabularySchemaManager.transformSchema(schema)));

        vocabularyDTO = new VocabularyDTO();
        vocabularyDTO.setId(1);
        vocabularyDTO.setSchemaId(schema.getId());
        vocabularyDTO.setName("Test vocabulary");
        vocabularyDTO.setDescription("Test vocabulary description");
//        vocabulary = vocabularyManager.create(vocabulary);
//        when(vocabularyRepository.findById(vocabulary.getId()))
//                .thenReturn(Optional.of(VocabularyManager.transformVocabulary(vocabulary, () -> schema)));
    }

    @Test
    void validateSetup() {
        VocabularyRecordDTO valid = new VocabularyRecordDTO();
        valid.setId(1);
        valid.setVocabularyId(vocabularyDTO.getId());

        FieldInstanceDTO nameField = new FieldInstanceDTO();
        nameField.setId(1);
        nameField.setDefinitionId(fdName.getId());
        nameField.setValue("TestName");

        valid.setFields(Set.of(nameField));

        recordManager.create(valid);
    }
}
