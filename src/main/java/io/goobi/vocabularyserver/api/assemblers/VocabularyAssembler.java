package io.goobi.vocabularyserver.api.assemblers;

import io.goobi.vocabularyserver.api.VocabularyController;
import io.goobi.vocabularyserver.exchange.VocabularyDTO;
import org.springframework.hateoas.EntityModel;
import org.springframework.hateoas.server.RepresentationModelAssembler;
import org.springframework.stereotype.Component;

import static org.springframework.hateoas.server.mvc.WebMvcLinkBuilder.linkTo;
import static org.springframework.hateoas.server.mvc.WebMvcLinkBuilder.methodOn;

@Component
public class VocabularyAssembler implements RepresentationModelAssembler<VocabularyDTO, EntityModel<VocabularyDTO>> {
    @Override
    public EntityModel<VocabularyDTO> toModel(VocabularyDTO entity) {
        return EntityModel.of(entity,
                linkTo(methodOn(VocabularyController.class).one(entity.getId())).withSelfRel(),
                linkTo(methodOn(VocabularyController.class).all()).withRel("vocabularies")
        );
    }
}
