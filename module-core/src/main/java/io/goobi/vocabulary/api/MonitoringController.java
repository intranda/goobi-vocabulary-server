package io.goobi.vocabulary.api;

import io.goobi.vocabulary.maintenance.MonitoringResult;
import io.goobi.vocabulary.service.manager.MaintenanceManager;
import lombok.extern.slf4j.Slf4j;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@Slf4j
@RestController
@RequestMapping("/api/v1")
public class MonitoringController {
    private final MaintenanceManager maintenanceManager;

    public MonitoringController(MaintenanceManager maintenanceManager) {
        this.maintenanceManager = maintenanceManager;
    }

    @GetMapping("/monitoring")
    public MonitoringResult monitoring() {
        return maintenanceManager.getMonitoringResult();
    }

}
