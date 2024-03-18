package io.goobi.vocabularyserver;

import io.goobi.vocabularyserver.model.FieldDefinition;
import io.goobi.vocabularyserver.model.FieldInstance;
import io.goobi.vocabularyserver.model.FieldType;
import io.goobi.vocabularyserver.model.Vocabulary;
import io.goobi.vocabularyserver.model.VocabularyRecord;
import io.goobi.vocabularyserver.model.VocabularySchema;
import io.goobi.vocabularyserver.repositories.FieldDefinitionRepository;
import io.goobi.vocabularyserver.repositories.FieldInstanceRepository;
import io.goobi.vocabularyserver.repositories.FieldTypeRepository;
import io.goobi.vocabularyserver.repositories.VocabularySchemaRepository;
import io.goobi.vocabularyserver.repositories.VocabularyRecordRepository;
import io.goobi.vocabularyserver.repositories.VocabularyRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.CommandLineRunner;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;

@SpringBootApplication
public class VocabularyServerApplication implements CommandLineRunner {

    public static void main(String[] args) {
        SpringApplication.run(VocabularyServerApplication.class, args);
    }

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

    private int vocabularyId;
    private long thorRecordId;

    @Override
    public void run(String... args) throws Exception {
        FieldType text = fieldTypeRepository.findById(1L).orElseGet(() -> fieldTypeRepository.save(new FieldType("text")));
        VocabularySchema vocabularySchema = new VocabularySchema();
//        schemaRepository.save(vocabularySchema);
        FieldDefinition heroName = new FieldDefinition(vocabularySchema, "Name", text);
        heroName.setMainEntry(true);
//		fieldDefinitionRepository.save(heroName);
        FieldDefinition heroPower = new FieldDefinition(vocabularySchema, "Power", text);
//		fieldDefinitionRepository.save(heroPower);
        vocabularySchema.getDefinitions().add(heroName);
        vocabularySchema.getDefinitions().add(heroPower);
        vocabularySchemaRepository.save(vocabularySchema);
        Vocabulary vocabulary = new Vocabulary(vocabularySchema, "MCU");
        vocabularyId = vocabularyRepository.save(vocabulary).getId();
        VocabularyRecord thor = new VocabularyRecord(vocabulary);
        FieldInstance thorName = new FieldInstance(heroName, thor, "Thor");
        FieldInstance thorPower = new FieldInstance(heroPower, thor, "8");
        thor.getFields().add(thorName);
        thor.getFields().add(thorPower);
        thorRecordId = vocabularyRecordRepository.save(thor).getId();
    }
}
