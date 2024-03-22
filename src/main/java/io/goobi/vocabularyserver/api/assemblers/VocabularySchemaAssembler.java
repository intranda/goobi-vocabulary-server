package io.goobi.vocabularyserver.api.assemblers;

import io.goobi.vocabularyserver.api.VocabularySchemaController;
import io.goobi.vocabularyserver.exchange.VocabularySchemaDTO;
import org.springframework.hateoas.EntityModel;
import org.springframework.hateoas.server.RepresentationModelAssembler;
import org.springframework.stereotype.Component;

import static org.springframework.hateoas.server.mvc.WebMvcLinkBuilder.linkTo;
import static org.springframework.hateoas.server.mvc.WebMvcLinkBuilder.methodOn;

@Component
public class VocabularySchemaAssembler implements RepresentationModelAssembler<VocabularySchemaDTO, EntityModel<VocabularySchemaDTO>> {
    @Override
    public EntityModel<VocabularySchemaDTO> toModel(VocabularySchemaDTO entity) {
        return EntityModel.of(entity,
                linkTo(methodOn(VocabularySchemaController.class).one(entity.getId())).withSelfRel(),
                linkTo(methodOn(VocabularySchemaController.class).all()).withRel("schemas")
        );
    }
}
