package io.goobi.vocabularyserver.api.assemblers;

import io.goobi.vocabularyserver.api.RecordController;
import io.goobi.vocabularyserver.exchange.VocabularyRecord;
import org.springframework.hateoas.EntityModel;
import org.springframework.hateoas.server.RepresentationModelAssembler;
import org.springframework.stereotype.Component;

import static org.springframework.hateoas.server.mvc.WebMvcLinkBuilder.linkTo;
import static org.springframework.hateoas.server.mvc.WebMvcLinkBuilder.methodOn;

@Component
public class RecordAssembler implements RepresentationModelAssembler<VocabularyRecord, EntityModel<VocabularyRecord>> {
    @Override
    public EntityModel<VocabularyRecord> toModel(VocabularyRecord entity) {
        return EntityModel.of(entity,
                linkTo(methodOn(RecordController.class).one(entity.getId())).withSelfRel(),
                linkTo(methodOn(RecordController.class).allInVocabulary(entity.getVocabularyId())).withRel("records")
        );
    }
}