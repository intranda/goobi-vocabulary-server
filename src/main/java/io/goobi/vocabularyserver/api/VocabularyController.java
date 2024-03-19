package io.goobi.vocabularyserver.api;


import io.goobi.vocabularyserver.exchange.Vocabulary;
import io.goobi.vocabularyserver.repositories.VocabularyRepository;
import io.goobi.vocabularyserver.service.exchange.ExchangeTypeTransformer;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RestController;

import java.util.List;
import java.util.stream.Collectors;

@RestController
public class VocabularyController {
    private final VocabularyRepository vocabularyRepository;
    private final ExchangeTypeTransformer exchangeTypeTransformer;

    VocabularyController(VocabularyRepository vocabularyRepository, ExchangeTypeTransformer exchangeTypeTransformer) {
        this.vocabularyRepository = vocabularyRepository;
        this.exchangeTypeTransformer = exchangeTypeTransformer;
    }

    @GetMapping("/vocabulary")
    List<Vocabulary> getAllVocabularies() {
        return vocabularyRepository.findAll()
                .stream()
                .map(exchangeTypeTransformer::transform)
                .collect(Collectors.toList());
    }
}
