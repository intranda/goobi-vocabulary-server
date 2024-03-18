package io.goobi.vocabularyserver.repositories;

import io.goobi.vocabularyserver.model.Schema;
import io.goobi.vocabularyserver.model.Vocabulary;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.jdbc.AutoConfigureTestDatabase;
import org.springframework.boot.test.autoconfigure.orm.jpa.DataJpaTest;
import org.springframework.test.context.ActiveProfiles;

import java.util.Optional;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

@DataJpaTest
@ActiveProfiles("test")
@AutoConfigureTestDatabase(replace = AutoConfigureTestDatabase.Replace.NONE)
class FullTest {
    @Autowired
    private SchemaRepository schemaRepository;
    @Autowired
    private VocabularyRepository vocabularyRepository;

    @BeforeEach
    public void setUp() {
        Schema schema = new Schema();
        schemaRepository.save(schema);
        Vocabulary vocabulary = new Vocabulary(schema, "Test vocabulary");
        vocabularyRepository.save(vocabulary);
    }

    @Test
    void testFetch() {
        Optional<Vocabulary> v = vocabularyRepository.findById(1);

        assertAll("all",
                () -> assertTrue(v.isPresent()),
                () -> assertEquals("Test vocabulary", v.get().getName())
        );
    }
}
