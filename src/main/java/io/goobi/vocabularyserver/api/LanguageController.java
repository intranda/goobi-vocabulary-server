package io.goobi.vocabularyserver.api;


import io.goobi.vocabularyserver.api.assemblers.LanguageAssembler;
import io.goobi.vocabularyserver.exception.IllegalAttributeProvidedException;
import io.goobi.vocabularyserver.exception.ValidationException;
import io.goobi.vocabularyserver.exchange.Language;
import io.goobi.vocabularyserver.model.LanguageEntity;
import io.goobi.vocabularyserver.service.manager.Manager;
import io.goobi.vocabularyserver.service.rdf.RDFMapper;
import org.springframework.data.domain.Pageable;
import org.springframework.data.web.PagedResourcesAssembler;
import org.springframework.hateoas.EntityModel;
import org.springframework.hateoas.PagedModel;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
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
        return pagedResourcesAssembler.toModel(DTOmanager.listAll(pageRequest), assembler);
    }

    @GetMapping("/languages/{id}")
    public EntityModel<Language> one(@PathVariable long id) {
        return assembler.toModel(DTOmanager.get(id));
    }

    @GetMapping(value = "/languages/{id}", produces = {"application/rdf+xml"})
    public String oneAsRdfXml(@PathVariable long id) {
        return rdfMapper.toRDFXML(managerEntity.get(id));
    }

    @GetMapping(value = "/languages/{id}", produces = {"application/n-triples", "text/turtle"})
    public String oneAsRdfTurtle(@PathVariable long id) {
        return rdfMapper.toRDFTurtle(managerEntity.get(id));
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
