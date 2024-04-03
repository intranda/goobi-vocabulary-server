package io.goobi.vocabularyserver.api.assemblers;

import io.goobi.vocabularyserver.api.LanguageController;
import io.goobi.vocabularyserver.exchange.LanguageDTO;
import org.springframework.hateoas.EntityModel;
import org.springframework.hateoas.server.RepresentationModelAssembler;
import org.springframework.stereotype.Component;

import static org.springframework.hateoas.server.mvc.WebMvcLinkBuilder.linkTo;
import static org.springframework.hateoas.server.mvc.WebMvcLinkBuilder.methodOn;

@Component
public class LanguageAssembler implements RepresentationModelAssembler<LanguageDTO, EntityModel<LanguageDTO>> {
    @Override
    public EntityModel<LanguageDTO> toModel(LanguageDTO entity) {
         return EntityModel.of(entity,
                linkTo(methodOn(LanguageController.class).one(entity.getId())).withSelfRel(),
                linkTo(methodOn(LanguageController.class).all(null, null)).withRel("languages"),
                linkTo(methodOn(LanguageController.class).delete(entity.getId())).withRel("delete")
        );
    }
}