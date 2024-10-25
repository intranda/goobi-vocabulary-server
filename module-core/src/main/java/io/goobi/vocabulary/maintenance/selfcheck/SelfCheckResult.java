package io.goobi.vocabulary.maintenance.selfcheck;

import java.util.Date;

public record SelfCheckResult(Date date, ValidationResult types, ValidationResult vocabularies, ValidationResult schemas, ValidationResult records) {
    public boolean success() {
        return types.errors().isEmpty() && vocabularies.errors().isEmpty() && schemas.errors().isEmpty() && records.errors().isEmpty();
    }
}
