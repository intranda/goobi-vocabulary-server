package io.goobi.vocabulary.maintenance;

import io.goobi.vocabulary.maintenance.selfcheck.SelfCheckResult;

public record MonitoringResult(MonitoringState monitoring, VersionsCollection versions, FlywayInformation flyway, SelfCheckResult selfCheck) {
}
