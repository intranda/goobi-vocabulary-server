package io.goobi.vocabulary.security;

import io.swagger.v3.oas.models.responses.ApiResponse;
import io.swagger.v3.oas.models.responses.ApiResponses;
import org.springdoc.core.customizers.OperationCustomizer;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

@Configuration
public class OpenApiSecurityResponsesConfig {

    @Bean
    OperationCustomizer addUnauthorizedResponse() {
        return (operation, handlerMethod) -> {
            ApiResponses responses = operation.getResponses();
            if (responses == null) {
                responses = new ApiResponses();
                operation.setResponses(responses);
            }
            responses.computeIfAbsent("401", code -> new ApiResponse()
                    .description("Authentication required"));
            return operation;
        };
    }
}
