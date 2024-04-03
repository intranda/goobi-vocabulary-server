package io.goobi.vocabularyserver.api;


import io.goobi.vocabularyserver.api.assemblers.FieldTypeAssembler;
import io.goobi.vocabularyserver.exception.ValidationException;
import io.goobi.vocabularyserver.exchange.FieldTypeDTO;
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
public class FieldTypeController {
    private final Manager<FieldTypeDTO> manager;
    private final FieldTypeAssembler assembler;

    public FieldTypeController(Manager<FieldTypeDTO> manager, FieldTypeAssembler assembler) {
        this.manager = manager;
        this.assembler = assembler;
    }

    @GetMapping("/types")
    public PagedModel<EntityModel<FieldTypeDTO>> all(Pageable pageRequest, PagedResourcesAssembler<FieldTypeDTO> pagedResourcesAssembler) {
        return pagedResourcesAssembler.toModel(manager.listAll(pageRequest), assembler);
    }

    @GetMapping("/types/{id}")
    public EntityModel<FieldTypeDTO> one(@PathVariable long id) {
        return assembler.toModel(manager.get(id));
    }

    @PostMapping("/types")
    @ResponseStatus(HttpStatus.CREATED)
    public EntityModel<FieldTypeDTO> create(@RequestBody FieldTypeDTO newFieldType) throws ValidationException {
        return assembler.toModel(manager.create(newFieldType));
    }

    @PutMapping("/types/{id}")
    @ResponseStatus(HttpStatus.OK)
    public EntityModel<FieldTypeDTO> update(@RequestBody FieldTypeDTO newFieldType, @PathVariable long id) {
        return assembler.toModel(manager.replace(newFieldType, id));
    }

    @DeleteMapping("/types/{id}")
    @ResponseStatus(HttpStatus.OK)
    public ResponseEntity<FieldTypeDTO> delete(@PathVariable long id) {
        return ResponseEntity.ok(manager.delete(id));
    }
}
