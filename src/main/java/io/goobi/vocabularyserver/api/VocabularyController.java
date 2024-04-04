package io.goobi.vocabularyserver.api;


import io.goobi.vocabularyserver.api.assemblers.VocabularyAssembler;
import io.goobi.vocabularyserver.exception.ValidationException;
import io.goobi.vocabularyserver.exchange.VocabularyDTO;
import io.goobi.vocabularyserver.service.manager.Manager;
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
import org.springframework.web.bind.annotation.ResponseStatus;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("/api/v1")
public class VocabularyController {
    private final Manager<VocabularyDTO> manager;
    private final VocabularyAssembler assembler;

    public VocabularyController(Manager<VocabularyDTO> manager, VocabularyAssembler assembler) {
        this.manager = manager;
        this.assembler = assembler;
    }

    @GetMapping("/vocabularies")
    public PagedModel<EntityModel<VocabularyDTO>> all(Pageable pageRequest, PagedResourcesAssembler<VocabularyDTO> pagedResourcesAssembler) {
        return pagedResourcesAssembler.toModel(manager.listAll(pageRequest), assembler);
    }

    @GetMapping("/vocabularies/{id}")
    public EntityModel<VocabularyDTO> one(@PathVariable long id) {
        return assembler.toModel(manager.get(id));
    }

    @PostMapping("/vocabularies")
    @ResponseStatus(HttpStatus.CREATED)
    public EntityModel<VocabularyDTO> create(@RequestBody VocabularyDTO vocabularyDTO) throws ValidationException {
        return assembler.toModel(manager.create(vocabularyDTO));
    }

    @PutMapping("/vocabularies/{id}")
    @ResponseStatus(HttpStatus.OK)
    public EntityModel<VocabularyDTO> update(@RequestBody VocabularyDTO vocabularyDTO, @PathVariable long id) throws ValidationException {
        return assembler.toModel(manager.replace(vocabularyDTO, id));
    }

    @DeleteMapping("/vocabularies/{id}")
    @ResponseStatus(HttpStatus.OK)
    public ResponseEntity<VocabularyDTO> delete(@PathVariable long id) {
        return ResponseEntity.ok(manager.delete(id));
    }
}
