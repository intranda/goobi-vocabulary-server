package io.goobi.vocabulary.maintenance;

import java.util.Date;

public record FlywayInformation(String schemaVersion, String description, Date date, int executionTime, boolean success) {
}
