package io.goobi.vocabularyserver.api;

import io.goobi.vocabularyserver.exchange.VocabularySchema;
import io.goobi.vocabularyserver.service.manager.VocabularySchemaManager;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RestController;

@RestController
public class SchemaController {
    private final VocabularySchemaManager manager;

    public SchemaController(VocabularySchemaManager manager) {
        this.manager = manager;
    }

    @GetMapping("/schema/{id}")
    VocabularySchema getSingleVocabularyById(@PathVariable long id) {
        return manager.get(id);
    }
}
