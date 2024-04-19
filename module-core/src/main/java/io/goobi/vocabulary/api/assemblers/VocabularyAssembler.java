package io.goobi.vocabulary.api.assemblers;

import io.goobi.vocabulary.api.VocabularyController;
import io.goobi.vocabulary.api.VocabularyRecordController;
import io.goobi.vocabulary.api.VocabularySchemaController;
import io.goobi.vocabulary.exchange.Vocabulary;
import org.springframework.hateoas.EntityModel;
import org.springframework.hateoas.server.RepresentationModelAssembler;
import org.springframework.stereotype.Component;

import static org.springframework.hateoas.server.mvc.WebMvcLinkBuilder.linkTo;
import static org.springframework.hateoas.server.mvc.WebMvcLinkBuilder.methodOn;

@Component
public class VocabularyAssembler implements RepresentationModelAssembler<Vocabulary, EntityModel<Vocabulary>> {
    @Override
    public EntityModel<Vocabulary> toModel(Vocabulary entity) {
        return EntityModel.of(entity,
                linkTo(methodOn(VocabularyController.class).one(entity.getId())).withSelfRel(),
                linkTo(methodOn(VocabularyController.class).all(null, null)).withRel("vocabularies"),
                linkTo(methodOn(VocabularySchemaController.class).one(entity.getSchemaId())).withRel("schema"),
                linkTo(methodOn(VocabularyRecordController.class).allInVocabulary(entity.getId(), null, null)).withRel("records"),
                linkTo(methodOn(VocabularyController.class).delete(entity.getId())).withRel("delete")
        );
    }
}
