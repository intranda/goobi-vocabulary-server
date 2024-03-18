package io.goobi.vocabularyserver.model;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.GenerationType;
import jakarta.persistence.Id;
import jakarta.persistence.JoinColumn;
import jakarta.persistence.OneToMany;
import jakarta.persistence.OneToOne;
import lombok.Getter;

import java.util.LinkedHashSet;
import java.util.Set;

@Entity
@Getter
public class Vocabulary {
    @Id
    @GeneratedValue(strategy = GenerationType.AUTO)
    @Column(name = "id", nullable = false)
    private Integer id;

    @OneToOne(optional = false)
    @JoinColumn(name = "schema_id", nullable = false)
    private Schema schema;

    @Column(name = "name", nullable = false)
    private String name;

    @Column(name = "description", length = 4096)
    private String description;

    @OneToMany(mappedBy = "vocabulary", orphanRemoval = true)
    private Set<Record> records = new LinkedHashSet<>();
}
