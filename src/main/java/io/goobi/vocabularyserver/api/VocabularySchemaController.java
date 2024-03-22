package io.goobi.vocabularyserver.api;

import io.goobi.vocabularyserver.api.assemblers.VocabularySchemaAssembler;
import io.goobi.vocabularyserver.exchange.VocabularySchema;
import io.goobi.vocabularyserver.service.manager.VocabularySchemaManager;
import org.springframework.hateoas.CollectionModel;
import org.springframework.hateoas.EntityModel;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import java.util.List;
import java.util.stream.Collectors;

import static org.springframework.hateoas.server.mvc.WebMvcLinkBuilder.linkTo;
import static org.springframework.hateoas.server.mvc.WebMvcLinkBuilder.methodOn;

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
    public CollectionModel<EntityModel<VocabularySchema>> all() {
        List<EntityModel<VocabularySchema>> schemas = manager.listAll().stream()
                .map(assembler::toModel)
                .collect(Collectors.toList());
        return CollectionModel.of(schemas,
                linkTo(methodOn(VocabularySchemaController.class).all()).withSelfRel());
    }

    @GetMapping("/schemas/{id}")
    public EntityModel<VocabularySchema> one(@PathVariable long id) {
        return assembler.toModel(manager.get(id));
    }
}
