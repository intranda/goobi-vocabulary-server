package io.goobi.vocabularyserver.model;

import jakarta.persistence.CascadeType;
import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.GenerationType;
import jakarta.persistence.Id;
import jakarta.persistence.JoinColumn;
import jakarta.persistence.ManyToOne;
import jakarta.persistence.OneToMany;
import lombok.Getter;
import lombok.Setter;

import java.util.LinkedList;
import java.util.List;

@Entity
@Getter
@Setter
public class Vocabulary {
    @Id
    @GeneratedValue(strategy = GenerationType.AUTO)
    @Column(name = "id", nullable = false)
    private long id;

    @ManyToOne(optional = false)
    @JoinColumn(name = "schema_id", nullable = false)
    private VocabularySchema schema;

    // `name` is a reserved Mysql keyword
    @Column(name = "title", nullable = false, unique = true)
    private String name;

    @Column(name = "description", length = 4096)
    private String description;

    @OneToMany(mappedBy = "vocabulary", cascade = CascadeType.ALL, orphanRemoval = true)
    private List<VocabularyRecord> records = new LinkedList<>();
}
