package io.goobi.vocabularyserver.api;

import io.goobi.vocabularyserver.exchange.VocabularySchema;
import io.goobi.vocabularyserver.repositories.VocabularySchemaRepository;
import io.goobi.vocabularyserver.service.exchange.ExchangeTypeTransformer;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RestController;

@RestController
public class SchemaController {
    private final VocabularySchemaRepository vocabularySchemaRepository;
    private final ExchangeTypeTransformer exchangeTypeTransformer;

    public SchemaController(VocabularySchemaRepository vocabularySchemaRepository, ExchangeTypeTransformer exchangeTypeTransformer) {
        this.vocabularySchemaRepository = vocabularySchemaRepository;
        this.exchangeTypeTransformer = exchangeTypeTransformer;
    }

    @GetMapping("/schema/{id}")
    VocabularySchema getSingleVocabularyById(@PathVariable long id) {
        // TODO: Make nice 404
        return exchangeTypeTransformer.transform(
                vocabularySchemaRepository.findById(id)
                        .orElseThrow()
        );
    }
}
