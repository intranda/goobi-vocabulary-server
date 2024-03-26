package io.goobi.vocabularyserver.api;

import io.goobi.vocabularyserver.api.assemblers.VocabularySchemaAssembler;
import io.goobi.vocabularyserver.exception.ValidationException;
import io.goobi.vocabularyserver.exchange.VocabularySchemaDTO;
import io.goobi.vocabularyserver.service.manager.VocabularySchemaManager;
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
public class VocabularySchemaController {
    private final VocabularySchemaManager manager;
    private final VocabularySchemaAssembler assembler;

    public VocabularySchemaController(VocabularySchemaManager manager, VocabularySchemaAssembler assembler) {
        this.manager = manager;
        this.assembler = assembler;
    }

    @GetMapping("/schemas")
    public CollectionModel<EntityModel<VocabularySchemaDTO>> all() {
        return assembler.toCollectionModel(manager.listAll());
    }

    @GetMapping("/schemas/{id}")
    public EntityModel<VocabularySchemaDTO> one(@PathVariable long id) {
        return assembler.toModel(manager.get(id));
    }

    @PostMapping("/schemas")
    @ResponseStatus(HttpStatus.CREATED)
    public EntityModel<VocabularySchemaDTO> create(@RequestBody VocabularySchemaDTO newSchema) throws ValidationException {
        return assembler.toModel(manager.create(newSchema));
    }

    @DeleteMapping("/schemas/{id}")
    @ResponseStatus(HttpStatus.OK)
    public ResponseEntity<VocabularySchemaDTO> delete(@PathVariable long id) {
        return ResponseEntity.ok(manager.delete(id));
    }
}
