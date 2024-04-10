package io.goobi.vocabularyserver.api;


import io.goobi.vocabularyserver.api.assemblers.VocabularyAssembler;
import io.goobi.vocabularyserver.exception.IllegalAttributeProvidedException;
import io.goobi.vocabularyserver.exception.ValidationException;
import io.goobi.vocabularyserver.exchange.Vocabulary;
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
    private final Manager<Vocabulary> manager;
    private final VocabularyAssembler assembler;

    public VocabularyController(Manager<Vocabulary> manager, VocabularyAssembler assembler) {
        this.manager = manager;
        this.assembler = assembler;
    }

    @GetMapping("/vocabularies")
    public PagedModel<EntityModel<Vocabulary>> all(Pageable pageRequest, PagedResourcesAssembler<Vocabulary> pagedResourcesAssembler) {
        return pagedResourcesAssembler.toModel(manager.listAll(pageRequest), assembler);
    }

    @GetMapping("/vocabularies/{id}")
    public EntityModel<Vocabulary> one(@PathVariable long id) {
        return assembler.toModel(manager.get(id));
    }

    @PostMapping("/vocabularies")
    @ResponseStatus(HttpStatus.CREATED)
    public EntityModel<Vocabulary> create(@RequestBody Vocabulary vocabularyDTO) throws ValidationException {
        return assembler.toModel(manager.create(vocabularyDTO));
    }

    @PutMapping("/vocabularies/{id}")
    @ResponseStatus(HttpStatus.OK)
    public EntityModel<Vocabulary> update(@RequestBody Vocabulary vocabularyDTO, @PathVariable long id) throws ValidationException {
        if (vocabularyDTO.getId() != null) {
            throw new IllegalAttributeProvidedException("id");
        }
        vocabularyDTO.setId(id);
        return assembler.toModel(manager.replace(vocabularyDTO));
    }

    @DeleteMapping("/vocabularies/{id}")
    @ResponseStatus(HttpStatus.OK)
    public ResponseEntity<Vocabulary> delete(@PathVariable long id) {
        return ResponseEntity.ok(manager.delete(id));
    }
}
