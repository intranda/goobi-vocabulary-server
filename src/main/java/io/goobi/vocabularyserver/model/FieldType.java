package io.goobi.vocabularyserver.model;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.GenerationType;
import jakarta.persistence.Id;
import jakarta.persistence.JoinColumn;
import jakarta.persistence.JoinTable;
import jakarta.persistence.ManyToMany;
import lombok.Getter;

import java.util.LinkedHashSet;
import java.util.Set;

@Entity
@Getter
public class FieldType {
    @Id
    @GeneratedValue(strategy = GenerationType.AUTO)
    @Column(name = "id", nullable = false)
    private Long id;

    @Column(name = "name", nullable = false, unique = true)
    private String name;

    @ManyToMany
    @JoinTable(name = "FieldType_selectableValues",
            joinColumns = @JoinColumn(name = "fieldType_id"),
            inverseJoinColumns = @JoinColumn(name = "selectableValues_id"))
    private Set<SelectableValue> selectableValues = new LinkedHashSet<>();
}
