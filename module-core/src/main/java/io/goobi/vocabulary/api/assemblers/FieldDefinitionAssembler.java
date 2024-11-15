package io.goobi.vocabulary.api.assemblers;

import io.goobi.vocabulary.api.FieldTypeController;
import io.goobi.vocabulary.api.VocabularySchemaController;
import io.goobi.vocabulary.exchange.FieldDefinition;
import org.springframework.hateoas.EntityModel;
import org.springframework.hateoas.server.RepresentationModelAssembler;
import org.springframework.stereotype.Component;

import static org.springframework.hateoas.server.mvc.WebMvcLinkBuilder.linkTo;
import static org.springframework.hateoas.server.mvc.WebMvcLinkBuilder.methodOn;

@Component
public class FieldDefinitionAssembler implements RepresentationModelAssembler<FieldDefinition, EntityModel<FieldDefinition>> {
    @Override
    public EntityModel<FieldDefinition> toModel(FieldDefinition entity) {
        return EntityModel.of(entity,
                linkTo(methodOn(FieldTypeController.class).one(entity.getId())).withSelfRel(),
                linkTo(methodOn(VocabularySchemaController.class).one(entity.getSchemaId())).withRel("schema")
        );
    }
}
