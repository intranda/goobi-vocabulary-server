package io.goobi.vocabularyserver.repositories;

import io.goobi.vocabularyserver.model.Field;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;

import java.util.Optional;

import static org.junit.jupiter.api.Assertions.assertEquals;

@SpringBootTest
class FieldRepositoryTest {
    @Autowired
    private FieldRepository fieldRepository;

    @BeforeEach
    public void setUp() {
        Field a = new Field("name", "Thor");

    }

    @Test
    void testFetch() {
        Optional<Field> result = fieldRepository.findById(1);
        assertEquals("Thor", result.orElseThrow().getName());
    }
}
