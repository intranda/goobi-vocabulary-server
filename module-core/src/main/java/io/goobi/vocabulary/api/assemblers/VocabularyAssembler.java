package io.goobi.vocabulary.api.assemblers;

import io.goobi.vocabulary.api.VocabularyController;
import io.goobi.vocabulary.api.VocabularyRecordController;
import io.goobi.vocabulary.api.VocabularySchemaController;
import io.goobi.vocabulary.exchange.Vocabulary;
import io.goobi.vocabulary.service.exchange.DTOMapper;
import io.goobi.vocabulary.service.rdf.RDFMapper;
import org.springframework.hateoas.EntityModel;
import org.springframework.hateoas.server.RepresentationModelAssembler;
import org.springframework.stereotype.Component;

import static org.springframework.hateoas.server.mvc.WebMvcLinkBuilder.linkTo;
import static org.springframework.hateoas.server.mvc.WebMvcLinkBuilder.methodOn;

@Component
public class VocabularyAssembler implements RepresentationModelAssembler<Vocabulary, EntityModel<Vocabulary>> {
    private final RDFMapper rdfMapper;
    private final DTOMapper dtoMapper;

    public VocabularyAssembler(RDFMapper rdfMapper, DTOMapper dtoMapper) {
        this.rdfMapper = rdfMapper;
        this.dtoMapper = dtoMapper;
    }

    @Override
    public EntityModel<Vocabulary> toModel(Vocabulary entity) {
        EntityModel<Vocabulary> result =
                EntityModel.of(entity,
                linkTo(methodOn(VocabularyController.class).one(entity.getId())).withSelfRel(),
                linkTo(methodOn(VocabularyController.class).list(null, null)).withRel("vocabularies"),
                linkTo(methodOn(VocabularySchemaController.class).one(entity.getSchemaId())).withRel("schema"),
                linkTo(methodOn(VocabularyRecordController.class).listInVocabulary(entity.getId(), null, null)).withRel("records"),
                linkTo(methodOn(VocabularyController.class).delete(entity.getId())).withRel("delete"),
                linkTo(methodOn(VocabularyController.class).exportAsJson(entity.getId())).withRel("export_json"),
                linkTo(methodOn(VocabularyController.class).exportAsCsv(entity.getId())).withRel("export_csv"),
                linkTo(methodOn(VocabularyController.class).exportAsExcel(entity.getId())).withRel("export_excel")
        );
        if (rdfMapper.isRDFCompatible(dtoMapper.toEntity(entity))) {
            result.add(linkTo(methodOn(VocabularyController.class).exportAsRdfXml(entity.getId())).withRel("export_rdf_xml"));
            result.add(linkTo(methodOn(VocabularyController.class).exportAsRdfTurtle(entity.getId())).withRel("export_rdf_turtle"));
        }
        if (entity.getMetadataSchemaId() != null) {
            result.add(linkTo(methodOn(VocabularySchemaController.class).one(entity.getMetadataSchemaId())).withRel("metadata_schema"));
        }
        return result;
    }
}
