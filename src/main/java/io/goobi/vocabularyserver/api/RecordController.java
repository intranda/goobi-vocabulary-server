package io.goobi.vocabularyserver.api;

import io.goobi.vocabularyserver.exchange.VocabularyRecord;
import io.goobi.vocabularyserver.service.manager.RecordManager;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import java.util.List;

@RestController
@RequestMapping("/api/v1")
public class RecordController {
    private final RecordManager manager;

    public RecordController(RecordManager manager) {
        this.manager = manager;
    }

    @GetMapping("/vocabularies/{vocabularyId}/records")
    List<VocabularyRecord> getAllRecordsForVocabulary(@PathVariable long vocabularyId) {
        return manager.listAllRecordsOfVocabulary(vocabularyId);
    }

    @PostMapping("/vocabularies/{vocabularyId}/records")
    VocabularyRecord createNewRecordForVocabulary(@PathVariable long vocabularyId, @RequestBody VocabularyRecord vocabularyRecord) {
        return manager.createNewRecordForVocabulary(vocabularyId, vocabularyRecord);
    }
}
