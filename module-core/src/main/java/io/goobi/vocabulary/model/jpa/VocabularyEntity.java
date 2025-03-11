package io.goobi.vocabulary.model.jpa;

import jakarta.persistence.CascadeType;
import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.GenerationType;
import jakarta.persistence.Id;
import jakarta.persistence.JoinColumn;
import jakarta.persistence.ManyToOne;
import jakarta.persistence.OneToMany;
import jakarta.persistence.Table;
import lombok.Getter;
import lombok.Setter;

import java.util.LinkedList;
import java.util.List;

@Entity
@Table(name = "vocabulary")
@Getter
@Setter
public class VocabularyEntity implements Identifiable {
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @Column(name = "id", nullable = false)
    private long id;

    @ManyToOne(optional = false)
    @JoinColumn(name = "schema_id", nullable = false)
    private VocabularySchemaEntity schema;

    @ManyToOne
    @JoinColumn(name = "metadata_schema_id")
    private VocabularySchemaEntity metadataSchema;

    // `name` is a reserved Mysql keyword
    @Column(name = "title", nullable = false, unique = true)
    private String name;

    @Column(name = "description", length = 4096)
    private String description;

    @OneToMany(mappedBy = "vocabulary", cascade = CascadeType.ALL, orphanRemoval = true)
    private List<VocabularyRecordEntity> records = new LinkedList<>();
}
