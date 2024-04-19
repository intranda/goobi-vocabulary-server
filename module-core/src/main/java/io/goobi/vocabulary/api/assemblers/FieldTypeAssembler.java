package io.goobi.vocabulary.api.assemblers;

import io.goobi.vocabulary.api.FieldTypeController;
import io.goobi.vocabulary.exchange.FieldType;
import org.springframework.hateoas.EntityModel;
import org.springframework.hateoas.server.RepresentationModelAssembler;
import org.springframework.stereotype.Component;

import static org.springframework.hateoas.server.mvc.WebMvcLinkBuilder.linkTo;
import static org.springframework.hateoas.server.mvc.WebMvcLinkBuilder.methodOn;

@Component
public class FieldTypeAssembler implements RepresentationModelAssembler<FieldType, EntityModel<FieldType>> {
    @Override
    public EntityModel<FieldType> toModel(FieldType entity) {
         return EntityModel.of(entity,
                linkTo(methodOn(FieldTypeController.class).one(entity.getId())).withSelfRel(),
                linkTo(methodOn(FieldTypeController.class).all(null, null)).withRel("types"),
                linkTo(methodOn(FieldTypeController.class).delete(entity.getId())).withRel("delete")
        );
    }
}