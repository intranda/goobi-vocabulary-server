package io.goobi.vocabularyserver.api.assemblers;

import io.goobi.vocabularyserver.api.VocabularyController;
import io.goobi.vocabularyserver.api.VocabularyRecordController;
import io.goobi.vocabularyserver.exchange.VocabularyRecordDTO;
import org.springframework.hateoas.EntityModel;
import org.springframework.hateoas.server.RepresentationModelAssembler;
import org.springframework.stereotype.Component;

import static org.springframework.hateoas.server.mvc.WebMvcLinkBuilder.linkTo;
import static org.springframework.hateoas.server.mvc.WebMvcLinkBuilder.methodOn;

@Component
public class RecordAssembler implements RepresentationModelAssembler<VocabularyRecordDTO, EntityModel<VocabularyRecordDTO>> {
    @Override
    public EntityModel<VocabularyRecordDTO> toModel(VocabularyRecordDTO entity) {
        return EntityModel.of(entity,
                linkTo(methodOn(VocabularyRecordController.class).one(entity.getId())).withSelfRel(),
                linkTo(methodOn(VocabularyController.class).one(entity.getVocabularyId())).withRel("vocabulary"),
                linkTo(methodOn(VocabularyRecordController.class).allInVocabulary(entity.getVocabularyId(), null, null)).withRel("vocabulary_records"),
                linkTo(methodOn(VocabularyRecordController.class).delete(entity.getId())).withRel("delete")
        );
    }
}