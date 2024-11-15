package io.goobi.vocabulary.monitoring;

import io.goobi.vocabulary.monitoring.SelfCheckResult;

public record MonitoringResult(MonitoringState monitoring, VersionsCollection versions, FlywayInformation flyway, SelfCheckResult selfCheck) {
}
