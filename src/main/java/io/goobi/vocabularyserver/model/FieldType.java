package io.goobi.vocabularyserver.model;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.GenerationType;
import jakarta.persistence.Id;
import jakarta.persistence.JoinColumn;
import jakarta.persistence.JoinTable;
import jakarta.persistence.ManyToMany;
import jakarta.persistence.ManyToOne;
import lombok.AccessLevel;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.NonNull;
import lombok.RequiredArgsConstructor;
import lombok.Setter;

import java.util.LinkedHashSet;
import java.util.Set;

@Entity
@Getter
@Setter
@RequiredArgsConstructor
@NoArgsConstructor(access = AccessLevel.PROTECTED)
public class FieldType {
    @Id
    @GeneratedValue(strategy = GenerationType.AUTO)
    @Column(name = "id", nullable = false)
    private Long id;

    // `name` is a reserved Mysql keyword
    @Column(name = "type_name", nullable = false, unique = true)
    @NonNull
    private String name;

    @Column(name = "validation")
    private String validation;

    @ManyToMany
    @JoinTable(name = "FieldType_selectableValues",
            joinColumns = @JoinColumn(name = "fieldType_id"),
            inverseJoinColumns = @JoinColumn(name = "selectableValues_id"))
    private Set<SelectableValue> selectableValues = new LinkedHashSet<>();
}
