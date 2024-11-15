package io.goobi.vocabulary.service.manager;

import com.fasterxml.jackson.databind.ObjectMapper;
import io.goobi.vocabulary.exception.VocabularyException;
import io.goobi.vocabulary.monitoring.FlywayInformation;
import io.goobi.vocabulary.service.maintenance.ManifestReader;
import io.goobi.vocabulary.monitoring.MonitoringResult;
import io.goobi.vocabulary.monitoring.MonitoringState;
import io.goobi.vocabulary.monitoring.VersionInformation;
import io.goobi.vocabulary.monitoring.VersionsCollection;
import io.goobi.vocabulary.monitoring.SelfCheckResult;
import io.goobi.vocabulary.monitoring.ValidationResult;
import io.goobi.vocabulary.model.jpa.FieldTypeEntity;
import io.goobi.vocabulary.model.jpa.VocabularyEntity;
import io.goobi.vocabulary.model.jpa.VocabularyRecordEntity;
import io.goobi.vocabulary.model.jpa.VocabularySchemaEntity;
import io.goobi.vocabulary.repositories.FieldTypeRepository;
import io.goobi.vocabulary.repositories.VocabularyRecordRepository;
import io.goobi.vocabulary.repositories.VocabularyRepository;
import io.goobi.vocabulary.repositories.VocabularySchemaRepository;
import io.goobi.vocabulary.validation.Validator;
import lombok.extern.slf4j.Slf4j;
import org.flywaydb.core.Flyway;
import org.springframework.data.repository.ListCrudRepository;
import org.springframework.stereotype.Service;

import java.io.File;
import java.io.IOException;
import java.util.Date;
import java.util.LinkedList;
import java.util.List;
import java.util.Optional;

@Slf4j
@Service
public class MaintenanceManager {
    public static final File SELF_CHECK_CACHE_FILE = new File("/tmp/vocabulary-server-self-check.json");

    private final FieldTypeRepository fieldTypeRepository;
    private final VocabularyRepository vocabularyRepository;
    private final VocabularySchemaRepository vocabularySchemaRepository;
    private final VocabularyRecordRepository vocabularyRecordRepository;

    private final Validator<FieldTypeEntity> fieldTypeValidator;
    private final Validator<VocabularyEntity> vocabularyValidator;
    private final Validator<VocabularySchemaEntity> vocabularySchemaValidator;
    private final Validator<VocabularyRecordEntity> vocabularyRecordValidator;

    private final ManifestReader manifestReader;
    private final Flyway flyway;

    private final ObjectMapper objectMapper;

    public MaintenanceManager(FieldTypeRepository fieldTypeRepository, VocabularyRepository vocabularyRepository, VocabularySchemaRepository vocabularySchemaRepository, VocabularyRecordRepository vocabularyRecordRepository, Validator<FieldTypeEntity> fieldTypeValidator, Validator<VocabularyEntity> vocabularyValidator, Validator<VocabularySchemaEntity> vocabularySchemaValidator, Validator<VocabularyRecordEntity> vocabularyRecordValidator, ManifestReader manifestReader, Flyway flyway, ObjectMapper objectMapper) {
        this.fieldTypeRepository = fieldTypeRepository;
        this.vocabularyRepository = vocabularyRepository;
        this.vocabularyValidator = vocabularyValidator;
        this.vocabularySchemaRepository = vocabularySchemaRepository;
        this.vocabularyRecordRepository = vocabularyRecordRepository;
        this.fieldTypeValidator = fieldTypeValidator;
        this.vocabularySchemaValidator = vocabularySchemaValidator;
        this.vocabularyRecordValidator = vocabularyRecordValidator;
        this.manifestReader = manifestReader;
        this.flyway = flyway;
        this.objectMapper = objectMapper;
    }

    public SelfCheckResult performFullSelfCheck() {
        ValidationResult types = new ValidationResult(selfTest(fieldTypeRepository, fieldTypeValidator));
        ValidationResult vocabularies = new ValidationResult(selfTest(vocabularyRepository, vocabularyValidator));
        ValidationResult vocabularySchemas = new ValidationResult(selfTest(vocabularySchemaRepository, vocabularySchemaValidator));
        ValidationResult vocabularyRecords = new ValidationResult(selfTest(vocabularyRecordRepository, vocabularyRecordValidator));

        SelfCheckResult result = new SelfCheckResult(new Date(), types, vocabularies, vocabularySchemas, vocabularyRecords);
        saveSelfCheckResult(result);
        return result;
    }

    private <Entity> List<String> selfTest(ListCrudRepository<Entity, Long> repo, Validator<Entity> validator) {
        List<String> errors = new LinkedList<>();
        for (Entity entity : repo.findAll()) {
            try {
                validator.validate(entity);
            } catch (VocabularyException e) {
                StringBuilder s = new StringBuilder();
                dumpException(s, e);
                errors.add(s.toString());
            }
        }
        return errors;
    }

    private void dumpException(StringBuilder s, VocabularyException ex) {
        s.append(ex.getMessage());
        if (ex.getCauses() != null) {
            for (VocabularyException c : ex.getCauses()) {
                s.append("; ");
                dumpException(s, c);
            }
        }
    }

    public MonitoringResult getMonitoringResult() {
        VersionInformation coreVersion = new VersionInformation(manifestReader.getVersion(), manifestReader.getRevision());
        VersionsCollection versions = new VersionsCollection(coreVersion);
        FlywayInformation flywayInformation = getFlywayInformation();
        Optional<SelfCheckResult> selfCheckResult = loadSelfCheckResult();
        MonitoringState monitoringState = getMonitoringState(flywayInformation, selfCheckResult);
        return new MonitoringResult(monitoringState, versions, flywayInformation, selfCheckResult.orElse(null));
    }

    private MonitoringState getMonitoringState(FlywayInformation flywayInformation, Optional<SelfCheckResult> selfCheckStatus) {
        String database = flywayInformation.success() ? "ok" : "error";
        String selfCheck = "unknown";
        if (selfCheckStatus.isPresent()) {
            selfCheck = selfCheckStatus.get().success() ? "ok" : "error";
        }
        return new MonitoringState(database, selfCheck);
    }

    private FlywayInformation getFlywayInformation() {
        String version = flyway.info().current().getVersion().getVersion();
        String description = flyway.info().current().getDescription();
        Date date = flyway.info().current().getInstalledOn();
        int executionTime = flyway.info().current().getExecutionTime();
        boolean success = flyway.info().current().getState().isApplied();
        return new FlywayInformation(version, description, date, executionTime, success);
    }

    private void saveSelfCheckResult(SelfCheckResult result) {
        try {
            objectMapper.writeValue(SELF_CHECK_CACHE_FILE, result);
        } catch (IOException e) {
            log.error("Error saving self-check result", e);
        }
    }

    private Optional<SelfCheckResult> loadSelfCheckResult() {
        if (!MaintenanceManager.SELF_CHECK_CACHE_FILE.exists()) {
            return Optional.empty();
        }
        try {
            return Optional.of(objectMapper.readValue(MaintenanceManager.SELF_CHECK_CACHE_FILE, SelfCheckResult.class));
        } catch (IOException e) {
            log.error("Error reading cached self-check results", e);
            return Optional.empty();
        }
    }
}
