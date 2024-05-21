package io.goobi.vocabulary.api;


import io.goobi.vocabulary.api.assemblers.VocabularyAssembler;
import io.goobi.vocabulary.exception.EntityNotFoundException;
import io.goobi.vocabulary.exception.IllegalAttributeProvidedException;
import io.goobi.vocabulary.exception.ValidationException;
import io.goobi.vocabulary.exchange.Vocabulary;
import io.goobi.vocabulary.repositories.VocabularyRepository;
import io.goobi.vocabulary.service.csv.CSVMapper;
import io.goobi.vocabulary.service.manager.VocabularyDTOManager;
import io.goobi.vocabulary.service.manager.VocabularyExportManager;
import io.goobi.vocabulary.service.rdf.RDFMapper;
import org.apache.commons.io.IOUtils;
import org.springframework.data.domain.Pageable;
import org.springframework.data.web.PagedResourcesAssembler;
import org.springframework.hateoas.CollectionModel;
import org.springframework.hateoas.EntityModel;
import org.springframework.hateoas.PagedModel;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.bind.annotation.ResponseStatus;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("/api/v1")
public class VocabularyController {
    private final VocabularyDTOManager manager;
    private final VocabularyExportManager exportManager;
    private final VocabularyAssembler assembler;
    private final RDFMapper rdfMapper;
    private final CSVMapper csvMapper;
    // TODO: Refactor VocabularyEntityManager for this
    private final VocabularyRepository vocabularyRepository;

    public VocabularyController(VocabularyDTOManager manager, VocabularyExportManager exportManager, VocabularyAssembler assembler, RDFMapper rdfMapper, CSVMapper csvMapper, VocabularyRepository vocabularyRepository) {
        this.manager = manager;
        this.exportManager = exportManager;
        this.assembler = assembler;
        this.rdfMapper = rdfMapper;
        this.csvMapper = csvMapper;
        this.vocabularyRepository = vocabularyRepository;
    }

    @GetMapping("/vocabularies/all")
    public CollectionModel<EntityModel<Vocabulary>> all() {
        return assembler.toCollectionModel(manager.listAll());
    }

    @GetMapping("/vocabularies")
    public PagedModel<EntityModel<Vocabulary>> list(Pageable pageRequest, PagedResourcesAssembler<Vocabulary> pagedResourcesAssembler) {
        return pagedResourcesAssembler.toModel(manager.list(pageRequest), assembler);
    }

    @GetMapping("/vocabularies/{id}")
    public EntityModel<Vocabulary> one(@PathVariable long id) {
        return assembler.toModel(manager.get(id));
    }

    @GetMapping("/vocabularies/find/{name}")
    public EntityModel<Vocabulary> findByName(@PathVariable String name) {
        return assembler.toModel(manager.find(name));
    }

    @GetMapping(
            value = "/vocabularies/{id}/export/json",
            produces = MediaType.APPLICATION_OCTET_STREAM_VALUE
    )
    public ResponseEntity<?> exportAsJson(@PathVariable long id) {
        return ResponseEntity.ok()
                .contentType(MediaType.parseMediaType("application/octet-stream"))
                .header("Content-disposition", "attachment; filename=\"vocabulary_" + id + ".json\"")
                .body(IOUtils.toByteArray(exportManager.export(id)));
    }

    @GetMapping(
            value = "/vocabularies/{id}/export/csv",
            produces = MediaType.APPLICATION_OCTET_STREAM_VALUE
    )
    public ResponseEntity<?> exportAsCsv(@PathVariable long id) {
        return ResponseEntity.ok()
                .contentType(MediaType.parseMediaType("application/octet-stream"))
                .header("Content-disposition", "attachment; filename=\"vocabulary_" + id + ".csv\"")
                .body(IOUtils.toByteArray(csvMapper.toCSV(vocabularyRepository.findById(id).orElseThrow(() -> new EntityNotFoundException(Vocabulary.class, id)))));
    }

    @GetMapping(
            value = "/vocabularies/{id}/export/rdfxml",
            produces = MediaType.APPLICATION_OCTET_STREAM_VALUE
    )
    public @ResponseBody ResponseEntity<?> exportAsRdfXml(@PathVariable long id) {
        return ResponseEntity.ok()
                .contentType(MediaType.parseMediaType("application/octet-stream"))
                .header("Content-disposition", "attachment; filename=\"vocabulary_" + id + ".rdf\"")
                .body(IOUtils.toByteArray(rdfMapper.toRDFXML(vocabularyRepository.findById(id).orElseThrow(() -> new EntityNotFoundException(Vocabulary.class, id)))));
    }

    @GetMapping(
            value = "/vocabularies/{id}/export/turtle",
            produces = MediaType.APPLICATION_OCTET_STREAM_VALUE
    )
    public @ResponseBody ResponseEntity<?> exportAsRdfTurtle(@PathVariable long id) {
        return ResponseEntity.ok()
                .contentType(MediaType.parseMediaType("application/octet-stream"))
                .header("Content-disposition", "attachment; filename=\"vocabulary_" + id + ".ttl\"")
                .body(IOUtils.toByteArray(rdfMapper.toRDFTurtle(vocabularyRepository.findById(id).orElseThrow(() -> new EntityNotFoundException(Vocabulary.class, id)))));
    }

    @PostMapping("/vocabularies")
    @ResponseStatus(HttpStatus.CREATED)
    public EntityModel<Vocabulary> create(@RequestBody Vocabulary vocabularyDTO) throws ValidationException {
        return assembler.toModel(manager.create(vocabularyDTO));
    }

    @PutMapping("/vocabularies/{id}")
    @ResponseStatus(HttpStatus.OK)
    public EntityModel<Vocabulary> update(@RequestBody Vocabulary vocabularyDTO, @PathVariable long id) throws ValidationException {
        if (vocabularyDTO.getId() != null) {
            throw new IllegalAttributeProvidedException("id");
        }
        vocabularyDTO.setId(id);
        return assembler.toModel(manager.replace(vocabularyDTO));
    }

    @DeleteMapping("/vocabularies/{id}")
    @ResponseStatus(HttpStatus.OK)
    public ResponseEntity<Vocabulary> delete(@PathVariable long id) {
        return ResponseEntity.ok(manager.delete(id));
    }
}
