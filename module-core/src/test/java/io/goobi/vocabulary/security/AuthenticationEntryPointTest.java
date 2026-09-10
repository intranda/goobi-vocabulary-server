package io.goobi.vocabulary.security;

import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.webmvc.test.autoconfigure.AutoConfigureMockMvc;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.http.HttpHeaders;
import org.springframework.test.web.servlet.MockMvc;

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

@SpringBootTest
@AutoConfigureMockMvc
class AuthenticationEntryPointTest {

    @Autowired
    private MockMvc mockMvc;

    @Test
    void missingAuthorizationHeader_returns401() throws Exception {
        mockMvc.perform(get("/api/v1/languages"))
                .andExpect(status().isUnauthorized());
    }

    @Test
    void invalidBearerToken_returns401() throws Exception {
        mockMvc.perform(get("/api/v1/languages")
                        .header(HttpHeaders.AUTHORIZATION, "Bearer wrong-token"))
                .andExpect(status().isUnauthorized());
    }

    @Test
    void validBearerToken_doesNotReturn401() throws Exception {
        mockMvc.perform(get("/api/v1/languages")
                        .header(HttpHeaders.AUTHORIZATION, "Bearer secret"))
                .andExpect(status().is2xxSuccessful());
    }

    @Test
    void openApiSpec_documentsUnauthorizedResponseOnEveryOperation() throws Exception {
        mockMvc.perform(get("/docs/api"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.paths['/api/v1/languages'].get.responses.401").exists())
                .andExpect(jsonPath("$.paths['/api/v1/vocabularies/{id}/export/csv'].get.responses.401").exists())
                .andExpect(jsonPath("$.paths['/api/v1/maintenance/selfcheck'].get.responses.401").exists());
    }
}
