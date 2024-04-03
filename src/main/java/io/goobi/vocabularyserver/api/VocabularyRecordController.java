package io.goobi.vocabularyserver.api;

import io.goobi.vocabularyserver.api.assemblers.RecordAssembler;
import io.goobi.vocabularyserver.exception.ValidationException;
import io.goobi.vocabularyserver.exchange.VocabularyRecordDTO;
import io.goobi.vocabularyserver.service.manager.RecordManager;
import org.springframework.data.domain.Pageable;
import org.springframework.data.web.PagedResourcesAssembler;
import org.springframework.hateoas.EntityModel;
import org.springframework.hateoas.PagedModel;
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
public class VocabularyRecordController {
    private final RecordManager manager;
    private final RecordAssembler assembler;

    public VocabularyRecordController(RecordManager manager, RecordAssembler assembler) {
        this.manager = manager;
        this.assembler = assembler;
    }

    @GetMapping("/vocabularies/{vocabularyId}/records")
    public PagedModel<EntityModel<VocabularyRecordDTO>> allInVocabulary(@PathVariable long vocabularyId, Pageable pageRequest, PagedResourcesAssembler<VocabularyRecordDTO> pagedResourcesAssembler) {
        return pagedResourcesAssembler.toModel(manager.listAll(vocabularyId, pageRequest), assembler);
    }

    @GetMapping("/records/{recordId}")
    public EntityModel<VocabularyRecordDTO> one(@PathVariable long recordId) {
        return assembler.toModel(manager.get(recordId));
    }

    @GetMapping("/vocabularies/{vocabularyId}/records/search")
    public PagedModel<EntityModel<VocabularyRecordDTO>> searchInVocabulary(@PathVariable long vocabularyId, @RequestBody String searchTerm, Pageable pageRequest, PagedResourcesAssembler<VocabularyRecordDTO> pagedResourcesAssembler) {
        return pagedResourcesAssembler.toModel(manager.search(vocabularyId, searchTerm, pageRequest), assembler);
    }

    @PostMapping("/vocabularies/{vocabularyId}/records")
    @ResponseStatus(HttpStatus.CREATED)
    public EntityModel<VocabularyRecordDTO> create(@PathVariable long vocabularyId, @RequestBody VocabularyRecordDTO vocabularyRecordDTO) throws ValidationException {
        if (vocabularyRecordDTO.getVocabularyId() != 0 && vocabularyRecordDTO.getVocabularyId() != vocabularyId) {
            throw new IllegalArgumentException("Inconsistency in passed id's");
        }
        if (vocabularyRecordDTO.getParentId() != null) {
            throw new IllegalArgumentException("You cannot provide parentId explicitly, please use the correct endpoint to add hierarchical records");
        }
        vocabularyRecordDTO.setVocabularyId(vocabularyId);
        return assembler.toModel(manager.create(vocabularyRecordDTO));
    }

    @PostMapping("/records/{recordId}")
    @ResponseStatus(HttpStatus.CREATED)
    public EntityModel<VocabularyRecordDTO> createSubRecord(@PathVariable long recordId, @RequestBody VocabularyRecordDTO vocabularyRecordDTO) throws ValidationException {
        if (vocabularyRecordDTO.getParentId() != null) {
            throw new IllegalArgumentException("You cannot provide parentId explicitly, it is set implicitly");
        }
        vocabularyRecordDTO.setParentId(recordId);
        return assembler.toModel(manager.createSubRecord(vocabularyRecordDTO));
    }

    @DeleteMapping("/records/{recordId}")
    @ResponseStatus(HttpStatus.OK)
    public ResponseEntity<VocabularyRecordDTO> delete(@PathVariable long recordId) {
        return ResponseEntity.ok(manager.delete(recordId));
    }
}
