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

import java.util.ArrayList;
import java.util.List;

@Entity
@Table(name = "vocabulary_schema")
@Getter
@Setter
public class VocabularySchemaEntity implements Identifiable {
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @Column(name = "id", nullable = false)
    private long id;

    @OneToMany(mappedBy = "schema", cascade = CascadeType.ALL, orphanRemoval = true)
    private List<FieldDefinitionEntity> definitions = new ArrayList<>();

    @Column(name = "hierarchical_records")
    private boolean hierarchicalRecords;

    @Column(name = "single_root_element")
    private boolean singleRootElement;
}
