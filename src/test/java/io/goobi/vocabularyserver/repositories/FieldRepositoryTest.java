package io.goobi.vocabularyserver.repositories;

import io.goobi.vocabularyserver.model.FieldDefinition;
import io.goobi.vocabularyserver.model.FieldInstance;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;

import java.util.Optional;

import static org.junit.jupiter.api.Assertions.assertEquals;

@SpringBootTest
class FieldRepositoryTest {
    @Autowired
    private FieldInstanceRepository fieldRepository;

    @BeforeEach
    public void setUp() {
        FieldDefinition d = new FieldDefinition();
        FieldInstance a = new FieldInstance(d, "Thor");
    }

    @Test
    void testFetch() {
        Optional<FieldInstance> result = fieldRepository.findById(1);
        assertEquals("Thor", result.orElseThrow().getValue());
    }
}
