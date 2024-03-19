package io.goobi.vocabularyserver.api;


import io.goobi.vocabularyserver.exchange.Vocabulary;
import io.goobi.vocabularyserver.service.manager.Manager;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RestController;

import java.util.List;

@RestController
public class VocabularyController {
    private final Manager<Vocabulary> manager;

    public VocabularyController(Manager<Vocabulary> manager) {
        this.manager = manager;
    }

    @GetMapping("/vocabulary")
    List<Vocabulary> getAllVocabularies() {
        return manager.listAll();
    }

    @PostMapping("/vocabulary")
    Vocabulary newVocabulary(@RequestBody Vocabulary vocabulary) {
        return manager.create(vocabulary);
    }

    // TODO: This is not working correctly: Impossible to pass id for replace insertion
    @PutMapping("/vocabulary")
    Vocabulary replaceVocabulary(@RequestBody Vocabulary vocabulary) {
        return manager.replace(vocabulary);
    }

    @DeleteMapping("/vocabulary/{id}")
    void deleteVocabulary(@PathVariable long id) {
        manager.delete(id);
    }
}
