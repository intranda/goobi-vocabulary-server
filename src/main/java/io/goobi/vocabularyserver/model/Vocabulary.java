package io.goobi.vocabularyserver.model;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.GenerationType;
import jakarta.persistence.Id;
import jakarta.persistence.JoinColumn;
import jakarta.persistence.OneToMany;
import jakarta.persistence.OneToOne;
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
@RequiredArgsConstructor
@NoArgsConstructor(access = AccessLevel.PROTECTED)
public class Vocabulary {
    @Id
    @GeneratedValue(strategy = GenerationType.AUTO)
    @Column(name = "id", nullable = false)
    private Integer id;

    @OneToOne(optional = false)
    @JoinColumn(name = "schema_id", nullable = false)
    @NonNull
    private Schema schema;

    @Column(name = "name", nullable = false)
    @NonNull
    private String name;

    @Column(name = "description", length = 4096)
    @Setter
    private String description;

    @OneToMany(mappedBy = "vocabulary", orphanRemoval = true)
    private Set<Record> records = new LinkedHashSet<>();
}
