package io.goobi.vocabulary.service.maintenance;

import lombok.Getter;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

@Getter
@Service
public class VersionInformation {
    @Value("${build.revision}")
    private String revision;
    @Value("${build.version}")
    private String version;
    @Value("${build.timestamp}")
    private String buildDate;
}
