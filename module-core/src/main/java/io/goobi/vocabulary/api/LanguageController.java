package io.goobi.vocabulary.api;


import io.goobi.vocabulary.api.assemblers.LanguageAssembler;
import io.goobi.vocabulary.exception.IllegalAttributeProvidedException;
import io.goobi.vocabulary.exception.ValidationException;
import io.goobi.vocabulary.exchange.Language;
import io.goobi.vocabulary.model.jpa.LanguageEntity;
import io.goobi.vocabulary.service.manager.Manager;
import io.goobi.vocabulary.service.rdf.RDFMapper;
import org.apache.commons.io.IOUtils;
import org.springframework.data.domain.Pageable;
import org.springframework.data.web.PagedResourcesAssembler;
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
public class LanguageController {
    private final Manager<Language> DTOmanager;
    private final Manager<LanguageEntity> managerEntity;
    private final LanguageAssembler assembler;
    private final RDFMapper rdfMapper;

    public LanguageController(Manager<Language> managerDTO, Manager<LanguageEntity> managerEntity, LanguageAssembler assembler, RDFMapper rdfMapper) {
        this.DTOmanager = managerDTO;
        this.managerEntity = managerEntity;
        this.assembler = assembler;
        this.rdfMapper = rdfMapper;
    }

    @GetMapping("/languages")
    public PagedModel<EntityModel<Language>> all(Pageable pageRequest, PagedResourcesAssembler<Language> pagedResourcesAssembler) {
        return pagedResourcesAssembler.toModel(DTOmanager.list(pageRequest), assembler);
    }

    @GetMapping("/languages/{id}")
    public EntityModel<Language> one(@PathVariable long id) {
        return assembler.toModel(DTOmanager.get(id));
    }

    @GetMapping(value = "/languages/{id}", produces = {"application/rdf+xml"})
    public String oneAsRdfXml(@PathVariable long id) {
        return rdfMapper.toRDFXML(managerEntity.get(id));
    }

    @GetMapping(
            value = "/languages/{id}/export/rdfxml",
            produces = MediaType.APPLICATION_OCTET_STREAM_VALUE
    )
    public @ResponseBody ResponseEntity<?> exportAsRdfXml(@PathVariable long id) {
        return ResponseEntity.ok()
                .contentType(MediaType.parseMediaType("application/octet-stream"))
                .header("Content-disposition", "attachment; filename=\"language_" + id + ".rdf\"")
                .body(IOUtils.toByteArray(rdfMapper.toRDFXML(managerEntity.get(id))));
    }

    @GetMapping(value = "/languages/{id}", produces = {"application/n-triples", "text/turtle"})
    public String oneAsRdfTurtle(@PathVariable long id) {
        return rdfMapper.toRDFTurtle(managerEntity.get(id));
    }

    @GetMapping(
            value = "/languages/{id}/export/turtle",
            produces = MediaType.APPLICATION_OCTET_STREAM_VALUE
    )
    public @ResponseBody ResponseEntity<?> exportAsRdfTurtle(@PathVariable long id) {
        return ResponseEntity.ok()
                .contentType(MediaType.parseMediaType("application/octet-stream"))
                .header("Content-disposition", "attachment; filename=\"language_" + id + ".ttl\"")
                .body(IOUtils.toByteArray(rdfMapper.toRDFTurtle(managerEntity.get(id))));
    }

    @PostMapping("/languages")
    @ResponseStatus(HttpStatus.CREATED)
    public EntityModel<Language> create(@RequestBody Language newLanguage) throws ValidationException {
        return assembler.toModel(DTOmanager.create(newLanguage));
    }

    @PutMapping("/languages/{id}")
    @ResponseStatus(HttpStatus.OK)
    public EntityModel<Language> update(@RequestBody Language newLanguage, @PathVariable long id) throws ValidationException {
        if (newLanguage.getId() != null) {
            throw new IllegalAttributeProvidedException("id");
        }
        newLanguage.setId(id);
        return assembler.toModel(DTOmanager.replace(newLanguage));
    }

    @DeleteMapping("/languages/{id}")
    @ResponseStatus(HttpStatus.OK)
    public ResponseEntity<Language> delete(@PathVariable long id) {
        return ResponseEntity.ok(DTOmanager.delete(id));
    }
}
