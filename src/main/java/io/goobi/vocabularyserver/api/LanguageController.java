package io.goobi.vocabularyserver.api;


import io.goobi.vocabularyserver.api.assemblers.LanguageAssembler;
import io.goobi.vocabularyserver.exception.IllegalAttributeProvidedException;
import io.goobi.vocabularyserver.exception.ValidationException;
import io.goobi.vocabularyserver.exchange.LanguageDTO;
import io.goobi.vocabularyserver.service.manager.Manager;
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
    private final Manager<LanguageDTO> manager;
    private final LanguageAssembler assembler;

    public LanguageController(Manager<LanguageDTO> manager, LanguageAssembler assembler) {
        this.manager = manager;
        this.assembler = assembler;
    }

    @GetMapping("/languages")
    public PagedModel<EntityModel<LanguageDTO>> all(Pageable pageRequest, PagedResourcesAssembler<LanguageDTO> pagedResourcesAssembler) {
        return pagedResourcesAssembler.toModel(manager.listAll(pageRequest), assembler);
    }

    @GetMapping("/languages/{id}")
    public EntityModel<LanguageDTO> one(@PathVariable long id) {
        return assembler.toModel(manager.get(id));
    }

    @PostMapping("/languages")
    @ResponseStatus(HttpStatus.CREATED)
    public EntityModel<LanguageDTO> create(@RequestBody LanguageDTO newLanguage) throws ValidationException {
        return assembler.toModel(manager.create(newLanguage));
    }

    @PutMapping("/languages/{id}")
    @ResponseStatus(HttpStatus.OK)
    public EntityModel<LanguageDTO> update(@RequestBody LanguageDTO newLanguage, @PathVariable long id) throws ValidationException {
        if (newLanguage.getId() != null) {
            throw new IllegalAttributeProvidedException("id");
        }
        newLanguage.setId(id);
        return assembler.toModel(manager.replace(newLanguage));
    }

    @DeleteMapping("/languages/{id}")
    @ResponseStatus(HttpStatus.OK)
    public ResponseEntity<LanguageDTO> delete(@PathVariable long id) {
        return ResponseEntity.ok(manager.delete(id));
    }
}
