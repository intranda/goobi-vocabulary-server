package io.goobi.vocabularyserver.api;


import io.goobi.vocabularyserver.exchange.Vocabulary;
import io.goobi.vocabularyserver.service.manager.Manager;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import java.util.List;

@RestController
@RequestMapping("/api/v1")
public class VocabularyController {
    private final Manager<Vocabulary> manager;

    public VocabularyController(Manager<Vocabulary> manager) {
        this.manager = manager;
    }

    @GetMapping("/vocabularies")
    List<Vocabulary> getAllVocabularies() {
        return manager.listAll();
    }

    @PostMapping("/vocabularies")
    Vocabulary newVocabulary(@RequestBody Vocabulary vocabulary) {
        return manager.create(vocabulary);
    }

    @PutMapping("/vocabularies/{id}")
    Vocabulary replaceVocabulary(@RequestBody Vocabulary vocabulary, @PathVariable long id) {
        return manager.replace(vocabulary, id);
    }

    @DeleteMapping("/vocabularies/{id}")
    void deleteVocabulary(@PathVariable long id) {
        manager.delete(id);
    }
}
