package io.goobi.vocabulary.service.manager;

import io.goobi.vocabulary.exception.VocabularyException;
import io.goobi.vocabulary.model.jpa.FieldTypeEntity;
import io.goobi.vocabulary.model.jpa.VocabularyEntity;
import io.goobi.vocabulary.model.jpa.VocabularyRecordEntity;
import io.goobi.vocabulary.model.jpa.VocabularySchemaEntity;
import io.goobi.vocabulary.repositories.FieldTypeRepository;
import io.goobi.vocabulary.repositories.VocabularyRecordRepository;
import io.goobi.vocabulary.repositories.VocabularyRepository;
import io.goobi.vocabulary.repositories.VocabularySchemaRepository;
import io.goobi.vocabulary.validation.Validator;
import org.springframework.data.repository.ListCrudRepository;
import org.springframework.stereotype.Service;

import java.util.LinkedList;
import java.util.List;

@Service
public class MaintenanceManager {
    private final FieldTypeRepository fieldTypeRepository;
    private final VocabularyRepository vocabularyRepository;
    private final VocabularySchemaRepository vocabularySchemaRepository;
    private final VocabularyRecordRepository vocabularyRecordRepository;

    private final Validator<FieldTypeEntity> fieldTypeValidator;
    private final Validator<VocabularyEntity> vocabularyValidator;
    private final Validator<VocabularySchemaEntity> vocabularySchemaValidator;
    private final Validator<VocabularyRecordEntity> vocabularyRecordValidator;

    public MaintenanceManager(FieldTypeRepository fieldTypeRepository, VocabularyRepository vocabularyRepository, VocabularySchemaRepository vocabularySchemaRepository, VocabularyRecordRepository vocabularyRecordRepository, Validator<FieldTypeEntity> fieldTypeValidator, Validator<VocabularyEntity> vocabularyValidator, Validator<VocabularySchemaEntity> vocabularySchemaValidator, Validator<VocabularyRecordEntity> vocabularyRecordValidator) {
        this.fieldTypeRepository = fieldTypeRepository;
        this.vocabularyRepository = vocabularyRepository;
        this.vocabularyValidator = vocabularyValidator;
        this.vocabularySchemaRepository = vocabularySchemaRepository;
        this.vocabularyRecordRepository = vocabularyRecordRepository;
        this.fieldTypeValidator = fieldTypeValidator;
        this.vocabularySchemaValidator = vocabularySchemaValidator;
        this.vocabularyRecordValidator = vocabularyRecordValidator;
    }

    public String testAllData() {
        List<String> selfCheckResults = new LinkedList<>();

        selfCheckResults.add(selfTest("Types", fieldTypeRepository, fieldTypeValidator));
        selfCheckResults.add(selfTest("Vocabularies", vocabularyRepository, vocabularyValidator));
        selfCheckResults.add(selfTest("Vocabulary Schemas", vocabularySchemaRepository, vocabularySchemaValidator));
        selfCheckResults.add(selfTest("Vocabulary Records", vocabularyRecordRepository, vocabularyRecordValidator));

        return String.join("\n", selfCheckResults);
    }


    private <Entity> String selfTest(String entityName, ListCrudRepository<Entity, Long> repo, Validator<Entity> validator) {
        List<String> errors = new LinkedList<>();
        for (Entity entity : repo.findAll()) {
            try {
                validator.validate(entity);
            } catch (VocabularyException e) {
                errors.add(dumpException(e, 1));
            }
        }
        if (errors.isEmpty()) {
            return entityName + ": OK";
        } else {
            return entityName + ": FAIL"
                    + String.join("", errors);
        }
    }

    private String dumpException(VocabularyException ex, int level) {
        StringBuilder s = new StringBuilder();
        s.append('\n')
                .append("\t".repeat(level))
                .append(ex.getMessage());
        if (ex.getCauses() != null) {
            for (VocabularyException c : ex.getCauses()) {
                s.append(dumpException(c, level + 1));
            }
        }
        return s.toString();
    }
}
