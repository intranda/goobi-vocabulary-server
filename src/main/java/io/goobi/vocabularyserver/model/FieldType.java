package io.goobi.vocabularyserver.model;

import jakarta.persistence.CascadeType;
import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.GenerationType;
import jakarta.persistence.Id;
import jakarta.persistence.OneToMany;
import lombok.Getter;
import lombok.Setter;

import java.util.LinkedHashSet;
import java.util.Set;

@Entity
@Getter
@Setter
public class FieldType {
    @Id
    @GeneratedValue(strategy = GenerationType.AUTO)
    @Column(name = "id", nullable = false)
    private Long id;

    // `name` is a reserved Mysql keyword
    @Column(name = "type_name", nullable = false, unique = true)
    private String name;

    @Column(name = "validation")
    private String validation;

    @OneToMany(mappedBy = "fieldType", cascade = CascadeType.ALL, orphanRemoval = true)
    private Set<SelectableValue> selectableValues = new LinkedHashSet<>();
}
