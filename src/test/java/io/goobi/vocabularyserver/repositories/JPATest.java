package io.goobi.vocabularyserver.repositories;

import io.goobi.vocabularyserver.model.FieldDefinition;
import io.goobi.vocabularyserver.model.FieldInstance;
import io.goobi.vocabularyserver.model.FieldType;
import io.goobi.vocabularyserver.model.VocabularySchema;
import io.goobi.vocabularyserver.model.Vocabulary;
import io.goobi.vocabularyserver.model.VocabularyRecord;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.jdbc.AutoConfigureTestDatabase;
import org.springframework.boot.test.autoconfigure.orm.jpa.DataJpaTest;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import java.util.Optional;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

@DataJpaTest
//This line makes changes to database persist after test
//@Transactional(propagation = Propagation.NOT_SUPPORTED)
@ActiveProfiles("test")
@AutoConfigureTestDatabase(replace = AutoConfigureTestDatabase.Replace.NONE)
class JPATest {
    @Autowired
    private FieldTypeRepository fieldTypeRepository;
    @Autowired
    private FieldDefinitionRepository fieldDefinitionRepository;
    @Autowired
    private VocabularySchemaRepository vocabularySchemaRepository;
    @Autowired
    private VocabularyRepository vocabularyRepository;
    @Autowired
    private FieldInstanceRepository fieldInstanceRepository;
    @Autowired
    private VocabularyRecordRepository vocabularyRecordRepository;

    private long vocabularyId;
    private long thorRecordId;

    @BeforeEach
    public void setUp() {
        FieldType text = fieldTypeRepository.findById(1L).orElse(new FieldType("text"));
        fieldTypeRepository.save(text);
        VocabularySchema vocabularySchema = vocabularySchemaRepository.findById(1L).orElse(new VocabularySchema());
//        vocabularySchemaRepository.save(vocabularySchema);
        FieldDefinition heroName = new FieldDefinition(vocabularySchema, "Name", text);
        heroName.setMainEntry(true);
//        fieldDefinitionRepository.save(heroName);
        FieldDefinition heroPower = new FieldDefinition(vocabularySchema, "Power", text);
//        fieldDefinitionRepository.save(heroPower);
        vocabularySchema.getDefinitions().add(heroName);
        vocabularySchema.getDefinitions().add(heroPower);
        vocabularySchemaRepository.save(vocabularySchema);
        Vocabulary vocabulary = vocabularyRepository.findByName("MCU").orElse(new Vocabulary(vocabularySchema, "MCU"));
        vocabularyId = vocabularyRepository.save(vocabulary).getId();
        VocabularyRecord thor = new VocabularyRecord(vocabulary);
        FieldInstance thorName = new FieldInstance(heroName, thor, "Thor");
        FieldInstance thorPower = new FieldInstance(heroPower, thor, "8");
        thor.getFields().add(thorName);
        thor.getFields().add(thorPower);
        thorRecordId = vocabularyRecordRepository.save(thor).getId();
    }

    @Test
    void testFetch() {
        Optional<Vocabulary> v = vocabularyRepository.findById(vocabularyId);
        assertAll("all", () -> assertTrue(v.isPresent()), () -> assertEquals("MCU", v.get().getName()));


        Optional<VocabularyRecord> thor = vocabularyRecordRepository.findById(thorRecordId);
        assertAll("all", () -> assertTrue(thor.isPresent()), () -> assertEquals("8", thor.get().getFields().stream().filter(f -> f.getDefinition().getName().equals("Power")).findAny().get().getValue()));
    }
}
