package io.goobi.vocabulary.api.assemblers;

import io.goobi.vocabulary.api.VocabularyController;
import io.goobi.vocabulary.api.VocabularyRecordController;
import io.goobi.vocabulary.exchange.VocabularyRecord;
import org.springframework.hateoas.EntityModel;
import org.springframework.hateoas.server.RepresentationModelAssembler;
import org.springframework.stereotype.Component;

import static org.springframework.hateoas.server.mvc.WebMvcLinkBuilder.linkTo;
import static org.springframework.hateoas.server.mvc.WebMvcLinkBuilder.methodOn;

@Component
public class RecordAssembler implements RepresentationModelAssembler<VocabularyRecord, EntityModel<VocabularyRecord>> {
    @Override
    public EntityModel<VocabularyRecord> toModel(VocabularyRecord entity) {
        EntityModel<VocabularyRecord> result = EntityModel.of(entity,
                linkTo(methodOn(VocabularyRecordController.class).one(entity.getId())).withSelfRel(),
                linkTo(methodOn(VocabularyController.class).one(entity.getVocabularyId())).withRel("vocabulary"),
//                linkTo(methodOn(VocabularyRecordController.class).listInVocabulary(entity.getVocabularyId(), null, null, null)).withRel("vocabulary_records"),  // TODO: optional parameter is encoded as {?search}, this leads to JSON parsing issues
                linkTo(methodOn(VocabularyRecordController.class).delete(entity.getId())).withRel("delete")
        );
        result.addIf(entity.getParentId() != null, () -> linkTo(methodOn(VocabularyRecordController.class).one(entity.getParentId())).withRel("parent_record"));
//        if (entity.getChildren() != null) {
//            entity.setChildren(entity.getChildren().stream().map(c -> toModel(Objects.requireNonNull(c.getContent()))).collect(Collectors.toSet()));
//        }
        return result;
    }
}