package io.goobi.vocabulary.api;


import io.goobi.vocabulary.api.assemblers.FieldTypeAssembler;
import io.goobi.vocabulary.exception.IllegalAttributeProvidedException;
import io.goobi.vocabulary.exception.ValidationException;
import io.goobi.vocabulary.exchange.FieldType;
import io.goobi.vocabulary.service.manager.FieldTypeDTOManager;
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
    private final FieldTypeDTOManager manager;
    private final FieldTypeAssembler assembler;

    public FieldTypeController(FieldTypeDTOManager manager, FieldTypeAssembler assembler) {
        this.manager = manager;
        this.assembler = assembler;
    }

    @GetMapping("/types")
    public PagedModel<EntityModel<FieldType>> all(Pageable pageRequest, PagedResourcesAssembler<FieldType> pagedResourcesAssembler) {
        return pagedResourcesAssembler.toModel(manager.list(pageRequest), assembler);
    }

    @GetMapping("/types/{id}")
    public EntityModel<FieldType> one(@PathVariable long id) {
        return assembler.toModel(manager.get(id));
    }

    @GetMapping("/types/find/{name}")
    public EntityModel<FieldType> findByName(@PathVariable String name) {
        return assembler.toModel(manager.find(name));
    }

    @PostMapping("/types")
    @ResponseStatus(HttpStatus.CREATED)
    public EntityModel<FieldType> create(@RequestBody FieldType newFieldType) throws ValidationException {
        return assembler.toModel(manager.create(newFieldType));
    }

    @PutMapping("/types/{id}")
    @ResponseStatus(HttpStatus.OK)
    public EntityModel<FieldType> update(@RequestBody FieldType newFieldType, @PathVariable long id) throws ValidationException {
        if (newFieldType.getId() != null) {
            throw new IllegalAttributeProvidedException("id");
        }
        newFieldType.setId(id);
        return assembler.toModel(manager.replace(newFieldType));
    }

    @DeleteMapping("/types/{id}")
    @ResponseStatus(HttpStatus.OK)
    public ResponseEntity<FieldType> delete(@PathVariable long id) {
        return ResponseEntity.ok(manager.delete(id));
    }
}
