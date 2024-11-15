package io.goobi.vocabulary.service.maintenance;

import lombok.Getter;
import org.apache.commons.lang3.StringUtils;
import org.springframework.stereotype.Service;

import java.io.IOException;
import java.util.Optional;
import java.util.jar.Attributes;
import java.util.jar.Manifest;

@Getter
@Service
public class ManifestReader {
    private final Manifest manifest;

    private final String revision;
    private final String version;
    private final String buildDate;

    public ManifestReader() throws IllegalArgumentException, IOException {
        manifest = new Manifest(getClass().getResourceAsStream("/META-INF/MANIFEST.MF"));
        Attributes mainAttributes = manifest.getMainAttributes();
        revision = getOptionalValue(mainAttributes, "Revision").orElse("unknown");
        version = getOptionalValue(mainAttributes, "Version").orElse("unknown");
        buildDate = getOptionalValue(mainAttributes, "Build-Date").orElse("unknown");
    }

    private Optional<String> getOptionalValue(Attributes attributes, String attributeName) throws IllegalArgumentException {
        String result = attributes.getValue(attributeName);
        if (StringUtils.isBlank(result)) {
            result = null;
        }
        return Optional.ofNullable(result);
    }
}
