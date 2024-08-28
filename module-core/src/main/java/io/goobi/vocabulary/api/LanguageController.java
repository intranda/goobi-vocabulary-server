package io.goobi.vocabulary.api;

import io.goobi.vocabulary.api.assemblers.LanguageAssembler;
import io.goobi.vocabulary.exception.IllegalAttributeProvidedException;
import io.goobi.vocabulary.exception.VocabularyException;
import io.goobi.vocabulary.exchange.Language;
import io.goobi.vocabulary.model.jpa.LanguageEntity;
import io.goobi.vocabulary.service.manager.LanguageDTOManager;
import io.goobi.vocabulary.service.manager.Manager;
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
    private final LanguageDTOManager manager;
    private final Manager<LanguageEntity> managerEntity;
    private final LanguageAssembler assembler;

    public LanguageController(LanguageDTOManager managerDTO, Manager<LanguageEntity> managerEntity, LanguageAssembler assembler) {
        this.manager = managerDTO;
        this.managerEntity = managerEntity;
        this.assembler = assembler;
    }

    @GetMapping("/languages")
    public PagedModel<EntityModel<Language>> all(Pageable pageRequest, PagedResourcesAssembler<Language> pagedResourcesAssembler) {
        return pagedResourcesAssembler.toModel(manager.list(pageRequest), assembler);
    }

    @GetMapping("/languages/{id}")
    public EntityModel<Language> one(@PathVariable long id) {
        return assembler.toModel(manager.get(id));
    }

    @GetMapping("/languages/by-abbreviation/{abbreviation}")
    public EntityModel<Language> findByAbbreviation(@PathVariable String abbreviation) {
        return assembler.toModel(manager.find(abbreviation));
    }

    @PostMapping("/languages")
    @ResponseStatus(HttpStatus.CREATED)
    public EntityModel<Language> create(@RequestBody Language newLanguage) throws VocabularyException {
        return assembler.toModel(manager.create(newLanguage));
    }

    @PutMapping("/languages/{id}")
    @ResponseStatus(HttpStatus.OK)
    public EntityModel<Language> update(@RequestBody Language newLanguage, @PathVariable long id) throws VocabularyException {
        if (newLanguage.getId() != null) {
            throw new IllegalAttributeProvidedException("id");
        }
        newLanguage.setId(id);
        return assembler.toModel(manager.replace(newLanguage));
    }

    @DeleteMapping("/languages/{id}")
    @ResponseStatus(HttpStatus.OK)
    public ResponseEntity<Language> delete(@PathVariable long id) {
        return ResponseEntity.ok(manager.delete(id));
    }
}
