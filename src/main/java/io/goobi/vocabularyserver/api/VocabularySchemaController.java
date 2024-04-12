package io.goobi.vocabularyserver.api;

import io.goobi.vocabularyserver.api.assemblers.VocabularySchemaAssembler;
import io.goobi.vocabularyserver.exception.IllegalAttributeProvidedException;
import io.goobi.vocabularyserver.exception.ValidationException;
import io.goobi.vocabularyserver.exchange.VocabularySchema;
import io.goobi.vocabularyserver.service.manager.VocabularySchemaDTOManager;
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
public class VocabularySchemaController {
    private final VocabularySchemaDTOManager manager;
    private final VocabularySchemaAssembler assembler;

    public VocabularySchemaController(VocabularySchemaDTOManager manager, VocabularySchemaAssembler assembler) {
        this.manager = manager;
        this.assembler = assembler;
    }

    @GetMapping("/schemas")
    public PagedModel<EntityModel<VocabularySchema>> all(Pageable pageRequest, PagedResourcesAssembler<VocabularySchema> pagedResourcesAssembler) {
        return pagedResourcesAssembler.toModel(manager.listAll(pageRequest), assembler);
    }

    @GetMapping("/schemas/{id}")
    public EntityModel<VocabularySchema> one(@PathVariable long id) {
        return assembler.toModel(manager.get(id));
    }

    @PostMapping("/schemas")
    @ResponseStatus(HttpStatus.CREATED)
    public EntityModel<VocabularySchema> create(@RequestBody VocabularySchema newSchema) throws ValidationException {
        return assembler.toModel(manager.create(newSchema));
    }

    @PutMapping("/schemas/{id}")
    @ResponseStatus(HttpStatus.OK)
    public EntityModel<VocabularySchema> update(@RequestBody VocabularySchema vocabularySchemaDTO, @PathVariable long id) {
        if (vocabularySchemaDTO.getId() != null) {
            throw new IllegalAttributeProvidedException("id");
        }
        vocabularySchemaDTO.setId(id);
        return assembler.toModel(manager.replace(vocabularySchemaDTO));
    }

    @DeleteMapping("/schemas/{id}")
    @ResponseStatus(HttpStatus.OK)
    public ResponseEntity<VocabularySchema> delete(@PathVariable long id) {
        return ResponseEntity.ok(manager.delete(id));
    }
}
