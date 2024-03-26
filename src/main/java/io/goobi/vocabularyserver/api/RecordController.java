package io.goobi.vocabularyserver.api;

import io.goobi.vocabularyserver.api.assemblers.RecordAssembler;
import io.goobi.vocabularyserver.exception.ValidationException;
import io.goobi.vocabularyserver.exchange.VocabularyRecordDTO;
import io.goobi.vocabularyserver.service.manager.RecordManager;
import org.springframework.hateoas.CollectionModel;
import org.springframework.hateoas.EntityModel;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.ResponseStatus;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("/api/v1")
public class RecordController {
    private final RecordManager manager;
    private final RecordAssembler assembler;

    public RecordController(RecordManager manager, RecordAssembler assembler) {
        this.manager = manager;
        this.assembler = assembler;
    }

    @GetMapping("/vocabularies/{vocabularyId}/records")
    public CollectionModel<EntityModel<VocabularyRecordDTO>> allInVocabulary(@PathVariable long vocabularyId) {
        return assembler.toCollectionModel(manager.listAll(vocabularyId));
    }

    @GetMapping("/records/{recordId}")
    public EntityModel<VocabularyRecordDTO> one(@PathVariable long recordId) {
        return assembler.toModel(manager.get(recordId));
    }

    @PostMapping("/vocabularies/{vocabularyId}/records")
    @ResponseStatus(HttpStatus.CREATED)
    public EntityModel<VocabularyRecordDTO> create(@PathVariable long vocabularyId, @RequestBody VocabularyRecordDTO vocabularyRecordDTO) throws ValidationException {
        if (vocabularyRecordDTO.getVocabularyId() != 0 && vocabularyRecordDTO.getVocabularyId() != vocabularyId) {
            throw new IllegalArgumentException("Inconsistency in passed id's");
        }
        vocabularyRecordDTO.setVocabularyId(vocabularyId);
        return assembler.toModel(manager.create(vocabularyRecordDTO));
    }

    @DeleteMapping("/records/{recordId}")
    @ResponseStatus(HttpStatus.OK)
    public ResponseEntity<VocabularyRecordDTO> delete(@PathVariable long recordId) {
        return ResponseEntity.ok(manager.delete(recordId));
    }
}
