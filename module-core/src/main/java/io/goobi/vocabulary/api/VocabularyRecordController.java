package io.goobi.vocabulary.api;

import io.goobi.vocabulary.api.assemblers.RecordAssembler;
import io.goobi.vocabulary.exception.IllegalAttributeProvidedException;
import io.goobi.vocabulary.exception.VocabularyException;
import io.goobi.vocabulary.exchange.VocabularyRecord;
import io.goobi.vocabulary.service.manager.RecordDTOManager;
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
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseStatus;
import org.springframework.web.bind.annotation.RestController;

import java.util.Collection;

@RestController
@RequestMapping("/api/v1")
public class VocabularyRecordController {
    private final RecordDTOManager manager;
    private final RecordAssembler assembler;

    public VocabularyRecordController(RecordDTOManager manager, RecordAssembler assembler) {
        this.manager = manager;
        this.assembler = assembler;
    }

    @GetMapping("/vocabularies/{vocabularyId}/records")
    public PagedModel<EntityModel<VocabularyRecord>> listInVocabulary(@PathVariable long vocabularyId, @RequestParam(required = false, name = "search") String search, @RequestParam(required = false, name = "all") Boolean all, Pageable pageRequest, PagedResourcesAssembler<VocabularyRecord> pagedResourcesAssembler) {
        if (Boolean.TRUE.equals(all)) {
            pageRequest = Pageable.unpaged(pageRequest.getSort());
        }
        if (search == null) {
            return pagedResourcesAssembler.toModel(manager.list(vocabularyId, pageRequest), assembler);
        } else {
            return pagedResourcesAssembler.toModel(manager.search(vocabularyId, search, pageRequest), assembler);
        }
    }

    @GetMapping("/records/{id}")
    public EntityModel<VocabularyRecord> one(@PathVariable long id) {
        return assembler.toModel(manager.get(id));
    }

    @GetMapping(value = "/records/{id}", produces = {"application/xml"})
    public VocabularyRecord oneAsXml(@PathVariable long id) {
        return manager.get(id);
    }

    @PostMapping("/vocabularies/{vocabularyId}/records")
    @ResponseStatus(HttpStatus.CREATED)
    public EntityModel<VocabularyRecord> create(@PathVariable long vocabularyId, @RequestBody VocabularyRecord vocabularyRecordDTO) throws VocabularyException {
        if (vocabularyRecordDTO.getVocabularyId() != null) {
            throw new IllegalAttributeProvidedException("vocabularyId");
        }
        if (vocabularyRecordDTO.getParentId() != null) {
            throw new IllegalAttributeProvidedException("parentId", "please use the correct endpoint to add hierarchical records");
        }
        vocabularyRecordDTO.setVocabularyId(vocabularyId);
        return assembler.toModel(manager.create(vocabularyRecordDTO));
    }

    @PostMapping("/records/{recordId}")
    @ResponseStatus(HttpStatus.CREATED)
    public EntityModel<VocabularyRecord> createSubRecord(@PathVariable long recordId, @RequestBody VocabularyRecord vocabularyRecordDTO) throws VocabularyException {
        if (vocabularyRecordDTO.getVocabularyId() != null) {
            throw new IllegalAttributeProvidedException("vocabularyId");
        }
        if (vocabularyRecordDTO.getParentId() != null) {
            throw new IllegalAttributeProvidedException("parentId", "it is set implicitly");
        }
        vocabularyRecordDTO.setParentId(recordId);
        return assembler.toModel(manager.createSubRecord(vocabularyRecordDTO));
    }

    @PutMapping("/records/{recordId}")
    @ResponseStatus(HttpStatus.OK)
    public EntityModel<VocabularyRecord> update(@RequestBody VocabularyRecord vocabularyRecordDTO, @PathVariable long recordId) throws VocabularyException {
        if (vocabularyRecordDTO.getId() != null) {
            throw new IllegalAttributeProvidedException("id");
        }
        vocabularyRecordDTO.setId(recordId);
        return assembler.toModel(manager.replace(vocabularyRecordDTO));
    }

    @DeleteMapping("/records/{recordId}")
    @ResponseStatus(HttpStatus.OK)
    public ResponseEntity<VocabularyRecord> delete(@PathVariable long recordId) {
        return ResponseEntity.ok(manager.delete(recordId));
    }

    @DeleteMapping("/vocabularies/{vocabularyId}/records")
    @ResponseStatus(HttpStatus.OK)
    public ResponseEntity<Collection<VocabularyRecord>> deleteAllRecords(@PathVariable long vocabularyId) {
        return ResponseEntity.ok(manager.deleteAllRecords(vocabularyId));
    }

    // Metadata routes

    @GetMapping("/vocabularies/{vocabularyId}/metadata")
    public EntityModel<VocabularyRecord> getMetadata(@PathVariable long vocabularyId) {
        return assembler.toModel(manager.getMetadata(vocabularyId));
    }

    @PostMapping("/vocabularies/{vocabularyId}/metadata")
    @ResponseStatus(HttpStatus.OK)
    public EntityModel<VocabularyRecord> createMetadata(@RequestBody VocabularyRecord vocabularyRecordDTO, @PathVariable long vocabularyId) throws VocabularyException {
        if (vocabularyRecordDTO.getMetadata() != null) {
            throw new IllegalAttributeProvidedException("metadata");
        }
        vocabularyRecordDTO.setMetadata(true);
        return create(vocabularyId, vocabularyRecordDTO);
    }

    @PutMapping("/vocabularies/{vocabularyId}/metadata")
    @ResponseStatus(HttpStatus.OK)
    public EntityModel<VocabularyRecord> updateMetadata(@RequestBody VocabularyRecord vocabularyRecordDTO, @PathVariable long vocabularyId) throws VocabularyException {
        if (vocabularyRecordDTO.getVocabularyId() != null) {
            throw new IllegalAttributeProvidedException("vocabularyId");
        }
        if (vocabularyRecordDTO.getMetadata() != null) {
            throw new IllegalAttributeProvidedException("metadata");
        }
        vocabularyRecordDTO.setVocabularyId(vocabularyId);
        vocabularyRecordDTO.setMetadata(true);
        return assembler.toModel(manager.replaceMetadata(vocabularyRecordDTO));
    }
}
