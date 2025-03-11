package io.goobi.vocabulary.model.jpa;

import jakarta.persistence.CascadeType;
import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.GenerationType;
import jakarta.persistence.Id;
import jakarta.persistence.OneToMany;
import jakarta.persistence.Table;
import lombok.Getter;
import lombok.Setter;

import java.util.LinkedList;
import java.util.List;

@Entity
@Table(name = "field_type")
@Getter
@Setter
public class FieldTypeEntity implements Identifiable {
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @Column(name = "id", nullable = false)
    private long id;

    // `name` is a reserved Mysql keyword
    @Column(name = "type_name", nullable = false, unique = true)
    private String name;

    @Column(name = "validation")
    private String validation;

    @Column(name = "large")
    private boolean large;

    @OneToMany(mappedBy = "fieldType", cascade = CascadeType.ALL, orphanRemoval = true)
    private List<SelectableValueEntity> selectableValues = new LinkedList<>();
}
