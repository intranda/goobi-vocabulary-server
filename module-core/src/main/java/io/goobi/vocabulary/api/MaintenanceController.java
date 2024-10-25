package io.goobi.vocabulary.api;

import io.goobi.vocabulary.service.manager.MaintenanceManager;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("/api/v1/maintenance")
public class MaintenanceController {
    private final MaintenanceManager maintenanceManager;

    public MaintenanceController(MaintenanceManager maintenanceManager) {
        this.maintenanceManager = maintenanceManager;
    }

    @GetMapping("/selfcheck")
    public ResponseEntity<String> selfCheck() {
        String status = maintenanceManager.testAllData();
        if (status.contains("FAIL")) {
            return ResponseEntity.internalServerError()
                    .body(status);
        } else {
            return ResponseEntity.ok()
                    .body(status);
        }
    }
}
