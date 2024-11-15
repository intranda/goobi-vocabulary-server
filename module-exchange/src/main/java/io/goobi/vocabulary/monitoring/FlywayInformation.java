package io.goobi.vocabulary.monitoring;

import java.util.Date;

public record FlywayInformation(String schemaVersion, String description, Date date, int executionTime, boolean success) {
}
