package io.goobi.vocabulary.maintenance.selfcheck;

import java.util.List;

public record ValidationResult(List<String> errors) {
}
