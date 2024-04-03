package io.goobi.vocabularyserver.api.assemblers;

import io.goobi.vocabularyserver.api.FieldTypeController;
import io.goobi.vocabularyserver.exchange.FieldTypeDTO;
import org.springframework.hateoas.EntityModel;
import org.springframework.hateoas.server.RepresentationModelAssembler;
import org.springframework.stereotype.Component;

import static org.springframework.hateoas.server.mvc.WebMvcLinkBuilder.linkTo;
import static org.springframework.hateoas.server.mvc.WebMvcLinkBuilder.methodOn;

@Component
public class FieldTypeAssembler implements RepresentationModelAssembler<FieldTypeDTO, EntityModel<FieldTypeDTO>> {
    @Override
    public EntityModel<FieldTypeDTO> toModel(FieldTypeDTO entity) {
         return EntityModel.of(entity,
                linkTo(methodOn(FieldTypeController.class).one(entity.getId())).withSelfRel(),
                linkTo(methodOn(FieldTypeController.class).all(null, null)).withRel("types"),
                linkTo(methodOn(FieldTypeController.class).delete(entity.getId())).withRel("delete")
        );
    }
}