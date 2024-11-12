package io.goobi.vocabulary.api;

import io.goobi.vocabulary.api.assemblers.FieldDefinitionAssembler;
import io.goobi.vocabulary.exchange.FieldDefinition;
import io.goobi.vocabulary.service.manager.FieldDefinitionDTOManager;
import org.springframework.hateoas.EntityModel;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("/api/v1")
public class FieldDefinitionController {
    private final FieldDefinitionDTOManager manager;
    private final FieldDefinitionAssembler assembler;

    public FieldDefinitionController(FieldDefinitionDTOManager manager, FieldDefinitionAssembler assembler) {
        this.manager = manager;
        this.assembler = assembler;
    }

    @GetMapping("/fieldDefinitions/{id}")
    public EntityModel<FieldDefinition> one(@PathVariable long id) {
        return assembler.toModel(manager.get(id));
    }
}
